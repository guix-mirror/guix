;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
;;; Copyright © 2015, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
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
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages check)
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
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://quassel-irc.org/pub/quassel-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "0mg8jydc70vlylppzich26q4s40kr78r3ysfyjwisfvlg2byxvs8"))
        (patches (search-patches "quassel-qt-514-compat.patch"))
        (modules '((guix build utils)))
        ;; We don't want to install the bundled inxi script.
        (snippet
         '(begin
            (delete-file "data/scripts/inxi")
            #t))))
    (build-system qt-build-system)
    (arguments
      ;; The three binaries are not mutually exlusive, and are all built
      ;; by default.
     '(#:configure-flags '(;;"-DWANT_QTCLIENT=OFF" ; 6.1 MiB
                           ;;"-DWANT_CORE=OFF" ; 3.0 MiB
                           ;;"-DWANT_MONO=OFF" ; 7.6 MiB
                           "-DWITH_KDE=OFF" ; no to kde integration ...
                           "-DWITH_BUNDLED_ICONS=ON" ; so we install bundled icons
                           "-DWITH_OXYGEN_ICONS=ON" ; also the oxygen ones
                           "-DWITH_WEBENGINE=OFF") ; we don't depend on qtwebengine
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-inxi-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((inxi (string-append (assoc-ref inputs "inxi") "/bin/inxi")))
               (symlink inxi "data/scripts/inxi")
               #t))))
       #:tests? #f)) ; no test target
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("inxi" ,inxi-minimal)
       ("libdbusmenu-qt" ,libdbusmenu-qt)
       ("qca" ,qca)
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)
       ("qtscript" ,qtscript)
       ("qtsvg" ,qtsvg)
       ("snorenotify" ,snorenotify)
       ("zlib" ,zlib)))
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
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/irssi/irssi/"
                                  "releases/download/" version "/irssi-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0y3mhnyr7x8ir8dlj83vbnchpfld28vdfni9yhpvls45j460c9v7"))))
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
                       (string-append "--with-proxy")
                       (string-append "--with-socks")
                       (string-append "--with-bot")))))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getcwd)))))))
    (inputs
     `(("glib" ,glib)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("perl" ,perl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://irssi.org/")
    (synopsis "Terminal-based IRC client")
    (description
     "Irssi is a terminal based IRC client for UNIX systems.  It also supports
SILC and ICB protocols via plugins.")
    (license license:gpl2+)))

(define-public weechat
  (package
    (name "weechat")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://weechat.org/files/src/weechat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1xx4fx7785yw3ml3k1z08h3qvrizvcypwl0j6jc7d7qim9sjjnm5"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ;; For tests.
       ("cpputest" ,cpputest)))
    (inputs
     `(("aspell" ,aspell)
       ("curl" ,curl)
       ("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt "out")
       ("ncurses" ,ncurses)
       ("zlib" ,zlib)

       ;; Scripting language plug-ins.
       ("guile" ,guile-3.0)
       ("lua" ,lua-5.1)
       ("perl" ,perl)
       ("python" ,python)
       ("tcl" ,tcl)))
    (arguments
     `(#:configure-flags
       (list "-DENABLE_PHP=OFF"
             "-DENABLE_RUBY=OFF"
             "-DENABLE_TESTS=ON")       ; ‘make test’ fails otherwise
       ;; Tests hang indefinitely on non-Intel platforms.
       #:tests? ,(if (any (cute string-prefix? <> (or (%current-target-system)
                                                      (%current-system)))
                          '("i686" "x86_64"))
                   '#t '#f)))
    (synopsis "Extensible chat client")
    (description "WeeChat (Wee Enhanced Environment for Chat) is an
@dfn{Internet Relay Chat} (IRC) client, which is designed to be light and fast.
The client uses a curses frontend, and there are remote interfaces for Web,
Qt, Android, and Emacs.

Everything in WeeChat can be done with the keyboard, though it also supports
using a mouse.  It is customizable and extensible with plugins and scripts.")
    (home-page "https://www.weechat.org/")
    (license license:gpl3)))

(define-public srain
  (package
    (name "srain")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SrainApp/srain")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vqjjsxzc4601dpc8lf9k25rp2c7sjab7l5a6cbfygpr8dqvm6vq"))))
    (arguments
     `(#:tests? #f ;there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'fix-permissions
           ;; Make po folder writable for gettext to install translations.
           (lambda _
             (for-each make-file-writable
                       (find-files "po" "." #:directories? #t)))))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("libconfig" ,libconfig)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("openssl" ,openssl)))
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
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lk8vjl7i8dcjh4jkg8h8bkapcbs465sy8g9c0chfqsywbmf3ndr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc")
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
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure
    (home-page "https://tools.suckless.org/sic/")
    (synopsis "Simple IRC client")
    (description
     "sic is a simple IRC client, even more minimalistic than ii.")
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
     `(("python-pytz" ,python-pytz)
       ("python-chardet" ,python-chardet)
       ("python-dateutil" ,python-dateutil)
       ("python-gnupg" ,python-gnupg)
       ("python-feedparser" ,python-feedparser)
       ("python-sqlalchemy" ,python-sqlalchemy)
       ("python-socksipy-branch" ,python-socksipy-branch)
       ("python-ecdsa" ,python-ecdsa)))
    (native-inputs
     `(("python-mock" ,python-mock)))
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
     `(("libressl" ,libressl)
       ("ncurses" ,ncurses)
       ("libarchive" ,libarchive) ; CHANGELOG: "Support for loading zip files"
       ("perl" ,perl)
       ("tcl" ,tcl)
       ("ruby" ,ruby)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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

(define-public inspircd
  (package
    (name "inspircd")
    (version "3.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/inspircd/inspircd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i30649dw84iscxa5as81g96f393mn1i883aq4za5ypdinr5x65g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (map (lambda (module)
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
       #:tests? #f ; Figure out later.
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'module-configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (apply invoke "./configure"
                    configure-flags)
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-lib (string-append out "/lib/"))
                    (out-bin (string-append out "/bin/"))
                    (out-etc (string-append out "/etc/"))
                    (name "inspircd"))
               (invoke "./configure"
                       (string-append "--prefix=" out-lib name)
                       (string-append "--binary-dir=" out-bin)
                       (string-append "--module-dir=" out-lib name "/modules/")
                       (string-append "--config-dir=" out-etc name)))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("argon2" ,argon2)
       ("gnutls" ,gnutls)
       ("libmaxminddb" ,libmaxminddb)
       ("mbedtls-apache" ,mbedtls-apache)
       ("mysql" ,mysql)
       ("openldap" ,openldap)
       ("openssl" ,openssl)
       ("pcre" ,pcre "bin")
       ("perl" ,perl)
       ("postgresql" ,postgresql)
       ("re2" ,re2)
       ("sqlite" ,sqlite)
       ("tre" ,tre)))
    (synopsis "Modular IRC server written in C++")
    (description "InspIRCd is a modular Internet Relay Chat
server written in C++ for Unix-like operating systems.")
    (home-page "https://www.inspircd.org/")
    (license license:gpl2)))
