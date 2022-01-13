;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
;;; Copyright © 2015, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 WinterHound <winterhound@yandex.com>
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
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public quassel
  (package
    (name "quassel")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://quassel-irc.org/pub/quassel-"
                            version ".tar.xz"))
        (sha256
         (base32
          "042fzssydvv35jjknziph8iyyjsyrsb2hp3d0ix0bqbagbrpf1q9"))
        (modules '((guix build utils)))
        ;; We don't want to install the bundled inxi script.
        (snippet
         '(begin
            (delete-file "data/scripts/inxi")))))
    (build-system qt-build-system)
    (arguments
      ;; The three binaries are not mutually exlusive, and are all built
      ;; by default.
     '(#:configure-flags '("-DBUILD_TESTING=ON"
                           ;;"-DWANT_QTCLIENT=OFF"
                           ;;"-DWANT_CORE=OFF"
                           ;;"-DWANT_MONO=OFF"
                           "-DWITH_KDE=OFF"
                           "-DWITH_BUNDLED_ICONS=ON"
                           "-DWITH_OXYGEN_ICONS=ON"
                           ;; This disables link previews.
                           "-DWITH_WEBENGINE=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-inxi-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((inxi (search-input-file inputs "/bin/inxi")))
               (symlink inxi "data/scripts/inxi")))))))
    (native-inputs
     (list extra-cmake-modules pkg-config qttools))
    (inputs
     (list boost
           inxi-minimal
           libdbusmenu-qt
           perl
           qca
           qtbase-5
           qtmultimedia
           qtscript
           qtsvg
           snorenotify
           sonnet
           zlib))
    (home-page "https://quassel-irc.org/")
    (synopsis "Distributed IRC client")
    (description "Quassel is a distributed IRC client, meaning that one or more
clients can attach to and detach from the central core.  It resembles the
popular combination of screen and a text-based IRC client such as WeeChat or
irssi, but graphical.")
    (license (list license:gpl2 license:gpl3)))) ;; dual licensed

(define-public irssi
  (package
    (name "irssi")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/irssi/irssi/"
                                  "releases/download/" version "/irssi-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "17vninwcdfxw39xl2q55qircckckjk2xlvkvlwgj5lhlxppvyix6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--enable-true-color")
                       (string-append "--with-proxy")
                       (string-append "--with-socks")
                       (string-append "--with-bot")))))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getcwd)))))))
    (inputs
     (list glib ncurses openssl perl utf8proc))
    (native-inputs
     (list pkg-config))
    (home-page "https://irssi.org/")
    (synopsis "Terminal-based IRC client")
    (description
     "Irssi is a terminal based IRC client for UNIX systems.  It also supports
SILC and ICB protocols via plugins.")
    (license license:gpl2+)))

(define-public weechat
  (package
    (name "weechat")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://weechat.org/files/src/weechat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0k5rgdy0c4dnxvsqjzyrr5czz1lmfk1vrsqkkvj8v24y0b3xrlvw"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc"))
    (native-inputs
     `(("gettext-minimal" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ,@(if (or (target-x86-64?)
                 (target-x86-32?))
           `(("ruby-asciidoctor" ,ruby-asciidoctor))
           '())
       ;; For tests.
       ("cpputest" ,cpputest)))
    (inputs
     (list aspell
           curl
           gnutls
           libgcrypt
           ncurses
           zlib
           ;; Scripting language plug-ins.
           guile-3.0
           lua-5.1
           perl
           python
           ruby
           tcl))
    (arguments
     `(#:configure-flags
       (list "-DENABLE_PHP=OFF"
             ,@(if (or (target-x86-64?)
                       (target-x86-32?))
                 '("-DENABLE_MAN=ON"
                   "-DENABLE_DOC=ON")
                '())
             "-DENABLE_TESTS=ON")       ; ‘make test’ fails otherwise
       #:phases
       (modify-phases %standard-phases
         ,@(if (or (target-x86-64?)
                   (target-x86-32?))
             '((add-after 'install 'move-doc
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                         (doc (assoc-ref outputs "doc"))
                         (from (string-append out "/share/doc/weechat"))
                         (to (string-append doc "/share/doc/weechat")))
                     (mkdir-p (string-append doc "/share/doc"))
                     (rename-file from to)))))
             '()))))
    (synopsis "Extensible chat client")
    (description "WeeChat (Wee Enhanced Environment for Chat) is an
@dfn{Internet Relay Chat} (IRC) client, which is designed to be light and fast.
The client uses a curses frontend, and there are remote interfaces for Web,
Qt, Android, and Emacs.

Everything in WeeChat can be done with the keyboard, though it also supports
using a mouse.  It is customizable and extensible with plugins and scripts.")
    (home-page "https://weechat.org/")
    (license license:gpl3)))

(define-public srain
  (package
    (name "srain")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SrainApp/srain")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xjk3fa3fkmsczif1bzcmi79k4z3jfdgcljfdiyn4iv5bh778swc"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f ;there are no tests
       #:glib-or-gtk? #t))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-sphinx" ,python-sphinx)))
    (inputs
     (list glib-networking
           gsettings-desktop-schemas
           gtk+
           libconfig
           libsecret
           libsoup-minimal-2
           openssl))
    (home-page "https://srain.im")
    (synopsis "Modern IRC client written in GTK")
    (description
     "Srain is an IRC client written in GTK.  It aims to be modern and easy to
use while still remaining useful to power users.  It also has partial support
for the IRCv3 protocol.")
    (license license:gpl3+)))

(define-public ircii
  (package
    (name "ircii")
    (version "20210314")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://ircii.warped.com/"
                                 name "-" version ".tar.gz"))
             (sha256
              (base32
               "04jczayv1vdn21fcf5zkfaa98sy7d6ydrv2sns2i67gvya2z28j3"))))
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
     (list ncurses openssl))
    (native-inputs
     (list pkg-config perl))
    (home-page "http://www.eterna.com.au/ircii/")
    (synopsis "Terminal-based IRC and ICB client")
    (description
     "ircII is a terminal based IRC and ICB client for UNIX systems.")
    (license license:bsd-3)))

(define-public catgirl
  (package
    (name "catgirl")
    (version "1.9a")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.causal.agency/catgirl/snapshot/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pci8crcgm33zb58y7ky2aydzyqsirj8ri8ik1zdlz6npadbjj9h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list (string-append "PREFIX=" %output)
                          ,(string-append "CC=" (cc-for-target)))))
    (native-inputs
     (list universal-ctags pkg-config))
    (inputs
     (list libressl ncurses))
    (home-page "https://git.causal.agency/catgirl")
    (synopsis "TLS-only terminal IRC client")
    (description
     "@command{catgirl} is a TLS-only terminal IRC client.

Notable features include:
@itemize
@item Tab complete: most recently seen or mentioned nicks are completed first.
  Commas are inserted between multiple nicks.
@item Prompt: the prompt clearly shows whether input will be interpreted as a
command or sent as a message.
@item Split scroll: keeps the latest messages in view while scrolling.
@item URL detection: recent URLs from a particular user or matching a
substring can be opened or copied.
@item Nick coloring: color generation based on usernames remains stable across
nick changes.  Mentions of users in messages are colored.
@item Topic diffing: the modified portion of a channel topic change is
highlighted.
@end itemize")
    (license license:gpl3+)))

(define-public ii
  (package
    (name "ii")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "05wcaszm9hap5gqf58bciqm3ad1kfgp976fs3fsn3ll3nliv6345"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "PREFIX=" %output)
                          ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure
    (home-page "https://tools.suckless.org/ii/")
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
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure
    (home-page "https://tools.suckless.org/sic/")
    (synopsis "Simple IRC client")
    (description
     "sic is a simple IRC client, even more minimalistic than ii.")
    (license license:expat)))

(define-public kirc
  (package
    (name "kirc")
    (version "0.2.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mcpcpc/kirc")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0ahmfxhgcvnlgmxxbv9vga5x6krab1n7qq55ygj7hj3x7s7ra419"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; No ./configure script
    (synopsis "IRC client written in POSIX C99")
    (description "Kirc is an Internet Relay Chat (IRC) client.  It includes
support for Simple Authentication and Security Layer (SASL), the
client-to-client (CTCP) protocol, simple chat history logging, synchronous
message handling, multi-channel joining at server connection, full support for
all RFC 2812 commands, and customized color scheme definitions.")
    (home-page "http://kirc.io/index.html")
    (license license:expat)))

(define-public limnoria
  (package
    (name "limnoria")
    (version "2019.11.22")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "limnoria" version))
       (sha256
        (base32 "0853xk1ps3v6lkmfx50wv56vynnzpl84v66hxnhl8i34zl36kk3c"))))
    (build-system python-build-system)
    (inputs
     (list python-pytz
           python-chardet
           python-dateutil
           python-gnupg
           python-feedparser
           python-sqlalchemy
           python-socksipy-branch
           python-ecdsa))
    (native-inputs
     (list python-mock))
    ;; Despite the existence of a test folder there is no test phase.
    ;; We need to package https://github.com/ProgVal/irctest and write
    ;; our own testphase.
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/ProgVal/Limnoria")
    (synopsis "Modified version of Supybot (an IRC bot and framework)")
    (description
     "Modified version of Supybot with Python 3 and IRCv3 support,
embedded web server, translations (fr, fi, it, hu, de), and many
other enhancements and bug fixes.")
    (license license:bsd-3)))

(define-public epic5
  (package
    (name "epic5")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.epicsol.org/pub/"
                                  "epic/EPIC5-PRODUCTION/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ap73d5f4vccxjaaq249zh981z85106vvqmxfm4plvy76b40y9jm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-perl
           (lambda _
             (substitute* "regress/crash-irc"
               (("perl5") (which "perl")))
             #t))
         (add-after 'unpack 'patch-bsdinstall
           ;; If we just remove /bin/ some part of the bsdinstall breaks.
           ;; Furthermore bsdinstalls has a reference to /etc/chmod here, which
           ;; means if we leave /etc/ in, install fails.
           (lambda _
             (substitute* "bsdinstall"
               (("/bin/strip") "strip")
               (("/bin/cp") "cp")
               (("/bin/chmod") "chmod")
               (("/bin/chgrp") "chgrp")
               (("/bin/mkdir") "mkdir")
               (("/bin/rm") "rm")
               (("/bin/mv") "mv")
               (("/etc/") ""))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The tarball uses a very old version of autconf. It does not
             ;; understand extra flags like `--enable-fast-install', so
             ;; we need to invoke it with just what it understands.
             (let ((out (assoc-ref outputs "out")))
               ;; 'configure' doesn't understand '--host'.
               ,@(if (%current-target-system)
                     `((setenv "CHOST" ,(%current-target-system)))
                     '())
               (setenv "CONFIG_SHELL" (which "bash"))
               (setenv "SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--with-ipv6" "--with-libarchive"
                       ;; We use libressl because openssl does not come
                       ;; with the lib/libssl.a which is needed for epic5.
                       ;; XXX: No matter which implementation is chosen,
                       ;; epic5 fails to connect to tls ports of roundrobin
                       ;; irc networks. This however is believed to be an
                       ;; protocol issue at epic5 related to ircd.
                       (string-append "--with-ssl="
                                      (assoc-ref %build-inputs "libressl"))
                       (string-append "--with-tcl="
                                      (assoc-ref %build-inputs "tcl")
                                      "/lib/tclConfig.sh"))))))))
    (inputs
     (list libressl
           ncurses
           libarchive ; CHANGELOG: "Support for loading zip files"
           perl
           tcl
           ruby))
    (native-inputs
     (list pkg-config))
    (home-page "http://epicsol.org")
    (synopsis "Epic5 IRC Client")
    (description
     "EPIC is a IRC client that has been under active development for
over 20 years.  It is stable and mature, and offers an excellent ircII
interface for those who are accustomed to the ircII way of doing things.")
    (license (list license:bsd-3
                   license:isc
                   license:bsd-4
                   ;; The epic license is equal to the standard three-clause
                   ;; BSD license except that you are not permitted to remove the
                   ;; "Redistribution is permitted" clause of the license if you
                   ;; distribute binaries.
                   (license:non-copyleft "http://epicsol.org/copyright")))))

(define-public litterbox
  (package
    (name "litterbox")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://git.causal.agency/litterbox/snapshot/litterbox-"
                           version ".tar.gz"))
       (sha256
        (base32 "0ll5d18slngdg2qhaxkvrcq2p1admh0h7sr06wx8347ka0vvrgjl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:make-flags
       (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" %output))))
    (native-inputs
      (list pkg-config universal-ctags))
    (inputs
      (list libressl sqlite))
    (home-page "https://code.causal.agency/june/litterbox")
    (synopsis "TLS-only IRC logger")
    (description
"@command{litterbox} is a TLS-only IRC logger.  It logs
events from IRC in a SQLite database, indexing messages for full-text
search.  It is intended for use with the IRC bouncer @command{pounce},
but can also be used independently as a logging bot.")
    (license license:gpl3+)))

(define-public inspircd
  (package
    (name "inspircd")
    (version "3.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/inspircd/inspircd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xlfs269iaw7dfryzl6vjzqsn2g4nqh6kpf5xfgk3zbjhqaczknx"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(map (lambda (module)
                    (string-append "--enable-extras=" module))
                  '("m_argon2.cpp"
                    "m_geo_maxmind.cpp"
                    "m_ldap.cpp"
                    "m_mysql.cpp"
                    "m_pgsql.cpp"
                    "m_regex_pcre.cpp"
                    "m_regex_posix.cpp"
                    "m_regex_stdlib.cpp"
                    "m_regex_re2.cpp"
                    "m_regex_tre.cpp"
                    "m_sqlite3.cpp"
                    "m_ssl_gnutls.cpp"
                    "m_ssl_openssl.cpp"
                    "m_ssl_mbedtls.cpp"
                    "m_sslrehashsignal.cpp"))
           #:tests? #f                  ; XXX figure out later
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'module-configure
                 (lambda* (#:key configure-flags #:allow-other-keys)
                   (apply invoke "./configure"
                          configure-flags)))
               (replace 'configure
                 (lambda _
                   (let ((lib (string-append #$output "/lib/"))
                         (bin (string-append #$output "/bin/"))
                         (etc (string-append #$output "/etc/"))
                         (name "inspircd"))
                     (invoke "./configure"
                             (string-append "--prefix=" lib name)
                             (string-append "--binary-dir=" bin)
                             (string-append "--module-dir=" lib name "/modules/")
                             (string-append "--config-dir=" etc name))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list argon2
           gnutls
           libmaxminddb
           mbedtls-apache
           mysql
           openldap
           openssl
           `(,pcre "bin")
           perl
           postgresql
           re2
           sqlite
           tre))
    (synopsis "Modular IRC server written in C++")
    (description "InspIRCd is a modular Internet Relay Chat
server written in C++ for Unix-like operating systems.")
    (home-page "https://www.inspircd.org/")
    (license license:gpl2)))
