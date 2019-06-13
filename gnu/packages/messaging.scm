;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016, 2017, 2018, 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
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

(define-module (gnu packages messaging)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages less)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public libotr
  (package
    (name "libotr")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://otr.cypherpunks.ca/libotr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1x8rliydhbibmzwdbyr7pd7n87m2jmxnqkpvaalnf4154hj1hfwb"))
              (patches (search-patches "libotr-test-auth-fix.patch"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgcrypt" ,libgcrypt)))  ; libotr headers include gcrypt.h
    (inputs `(("libgpg-error" ,libgpg-error)))
    (native-inputs `(("perl" ,perl))) ; for the test suite
    (synopsis "Off-the-Record (OTR) Messaging Library and Toolkit")
    (description
     "OTR allows you to have private conversations over instant messaging by
providing: (1) Encryption: No one else can read your instant messages.  (2)
Authentication: You are assured the correspondent is who you think it is.  (3)
Deniability: The messages you send do not have digital signatures that are
checkable by a third party.  Anyone can forge messages after a conversation to
make them look like they came from you.  However, during a conversation, your
correspondent is assured the messages he sees are authentic and
unmodified.  (4) Perfect forward secrecy: If you lose control of your private
keys, no previous conversation is compromised.")
    (home-page "https://otr.cypherpunks.ca/")
    (license (list license:lgpl2.1 license:gpl2))))

(define-public libsignal-protocol-c
  (package
  (name "libsignal-protocol-c")
  (version "2.3.2")
  (source (origin
           (method url-fetch)
           (uri (string-append "https://github.com/WhisperSystems/"
                               "libsignal-protocol-c/archive/v" version
                               ".tar.gz"))
           (file-name (string-append name "-" version ".tar.gz"))
           (sha256
            (base32
             "0380hl6fw3ppf265fg897pyrpqygpx4m9j8ifq118bim8lq6z0pk"))))
  (arguments
   `(;; Required for proper linking and for tests to run.
     #:configure-flags '("-DBUILD_SHARED_LIBS=on" "-DBUILD_TESTING=1")))
  (build-system cmake-build-system)
  (inputs `( ;; Required for tests:
            ("check" ,check)
            ("openssl" ,openssl)))
  (native-inputs `(("pkg-config" ,pkg-config)))
  (home-page "https://github.com/WhisperSystems/libsignal-protocol-c")
  (synopsis "Implementation of a ratcheting forward secrecy protocol")
  (description "libsignal-protocol-c is an implementation of a ratcheting
forward secrecy protocol that works in synchronous and asynchronous
messaging environments.  It can be used with messaging software to provide
end-to-end encryption.")
  (license license:gpl3+)))

(define-public bitlbee
  (package
    (name "bitlbee")
    (version "3.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://get.bitlbee.org/src/bitlbee-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0sgsn0fv41rga46mih3fyv65cvfa6rvki8x92dn7bczbi7yxfdln"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("check" ,check)))
    (inputs `(("glib" ,glib)
              ("libotr" ,libotr)
              ("gnutls" ,gnutls)
              ("python" ,python-2)
              ("perl" ,perl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-etc
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "install-etc" make-flags)))
         (add-after 'install-etc 'install-lib
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "install-dev" make-flags)))
         (replace 'configure
           ;; bitlbee's configure script does not tolerate many of the
           ;; variable settings that Guix would pass to it.
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./configure"
                     (string-append "--prefix="
                                    (assoc-ref outputs "out"))
                     "--otr=1"))))))
    (synopsis "IRC to instant messaging gateway")
    (description "BitlBee brings IM (instant messaging) to IRC clients, for
people who have an IRC client running all the time and don't want to run an
additional IM client.  BitlBee currently supports XMPP/Jabber (including
Google Talk), MSN Messenger, Yahoo!  Messenger, AIM and ICQ, and the Twitter
microblogging network (plus all other Twitter API compatible services like
identi.ca and status.net).")
    (home-page "http://www.bitlbee.org/")
    (license (list license:gpl2+ license:bsd-2))))

(define-public bitlbee-discord
  (package
    (name "bitlbee-discord")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sm00th/bitlbee-discord.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02pigk2vbz0jdz11f96sygdvp1j762yjn62h124fkcsc070g7a2f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-autogen
           (lambda _
             (let ((sh (which "sh")))
               (substitute* "autogen.sh" (("/bin/sh") sh))
               (setenv "CONFIG_SHELL" sh))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "./configure"
                     (string-append "--with-plugindir="
                                    (assoc-ref outputs "out")
                                    "/lib/bitlbee/")))))))
    (inputs `(("glib" ,glib)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("texinfo" ,texinfo)
                     ("libtool" ,libtool)
                     ("bitlbee" ,bitlbee) ; needs bitlbee headers
                     ("bash" ,bash)))
    (synopsis "Discord plugin for Bitlbee")
    (description "Bitlbee-discord is a plugin for Bitlbee witch provides
access to servers running the Discord protocol.")
    (home-page "https://github.com/sm00th/bitlbee-discord/")
    (license license:gpl2+)))

(define-public hexchat
  (package
    (name "hexchat")
    (version "2.14.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.hexchat.net/hexchat/hexchat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "064nq151nzsljv97dmkifyl162d2738vbgvm1phx7yv04pjvk4kp"))))
    (build-system meson-build-system)
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("perl" ,perl)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("dbus-glib" ,dbus-glib)
              ("dbus" ,dbus)
              ("enchant" ,enchant)
              ("glib:bin" ,glib "bin")            ;need glib-genmarshal
              ("gtk" ,gtk+-2)
              ("libcanberra" ,libcanberra)
              ("libnotify" ,libnotify)
              ("libproxy" ,libproxy)
              ("openssl" ,openssl)

              ;; Bindings for add-on scripts.
              ("luajit" ,luajit)
              ("perl-xml-parser" ,perl-xml-parser)
              ("python-2" ,python-2)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-desktop-database-updates
           (lambda _
             ;; The build scripts update icon and desktop file databases when
             ;; DESTDIR is not set.  We can't update these databases from
             ;; within the build chroot, but we also don't set DESTDIR.  So, we
             ;; just skip this code.
             (substitute* "meson_post_install.py"
               (("if 'DESTDIR' not in os.environ:")
                 "if False:"))
             #t)))))
    (synopsis "Graphical IRC Client")
    (description
     "HexChat lets you connect to multiple IRC networks at once.  The main
window shows the list of currently connected networks and their channels, the
current conversation and the list of users.  It uses colors to differentiate
between users and to highlight messages.  It checks spelling using available
dictionaries.  HexChat can be extended with multiple addons.")
    (home-page "https://hexchat.net/")
    (license license:gpl2+)))

(define-public ngircd
  (package
    (name "ngircd")
    (version "25")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://arthur.barton.de/pub/ngircd/ngircd-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0kpf5qi98m9f833r4rx9n6h9p31biwk798jwc1mgzmix7sp7r6f4"))
              (patches (search-patches "ngircd-handle-zombies.patch"))))
    (build-system gnu-build-system)
    ;; Needed for the test suite.
    (native-inputs `(("procps" ,procps)
                     ("expect" ,expect)
                     ("inetutils" ,inetutils)))
    ;; XXX Add libident.
    (inputs `(("zlib" ,zlib)
              ("libwrap" ,tcp-wrappers)
              ("gnutls" ,gnutls)
              ,@(if (string-suffix? "-linux"
                                    (or (%current-target-system)
                                        (%current-system)))
                    `(("linux-pam" ,linux-pam))
                    '())))
    (arguments
     `(#:configure-flags
       '("--with-gnutls" "--with-iconv" "--enable-ipv6" "--with-tcp-wrappers"
         ,@(if (string-suffix? "-linux"
                               (or (%current-target-system)
                                   (%current-system)))
               '("--with-pam")
               '()))
       #:phases
       (modify-phases %standard-phases
         ;; Necessary for the test suite.
         (add-after 'configure 'post-configure
           (lambda _
             (substitute* "src/ngircd/Makefile"
               (("/bin/sh") (which "sh")))
             ;; The default getpid.sh does a sloppy grep over 'ps -ax' output,
             ;; which fails arbitrarily.
             (with-output-to-file "src/testsuite/getpid.sh"
               (lambda ()
                 (display
                  (string-append
                   "#!" (which "sh") "\n"
                   "ps -C \"$1\" -o pid=\n"))))
             ;; Our variant of getpid.sh does not match interpreter names
             ;; when the script's shebang is invoked directly as "./foo".
             ;; Patch cases where the test suite relies on this.
             (substitute* "src/testsuite/start-server.sh"
               ;; It runs 'getpid.sh sh' to test if it works at all.  Run it on
               ;; 'make' instead.
               (("getpid.sh sh") "getpid.sh make")))))))
    (home-page "https://ngircd.barton.de/")
    (synopsis "Lightweight Internet Relay Chat server for small networks")
    (description
     "ngIRCd is a lightweight @dfn{Internet Relay Chat} (IRC) server for small
or private networks.  It is easy to configure, can cope with dynamic IP
addresses, and supports IPv6, SSL-protected connections, as well as PAM for
authentication.")
    (license license:gpl2+)))

(define-public pidgin
  (package
    (name "pidgin")
    (version "2.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pidgin/Pidgin/"
                           version "/pidgin-" version ".tar.bz2"))
       (sha256
        (base32 "13vdqj70315p9rzgnbxjp9c51mdzf1l4jg1kvnylc4bidw61air7"))
       (patches (search-patches "pidgin-add-search-path.patch"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("check" ,check)
       ("intltool" ,intltool)
       ("gconf" ,gconf)
       ("python" ,python-2)
       ("doxygen" ,doxygen)))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("libgcrypt" ,libgcrypt)
       ("gnutls" ,gnutls)
       ("cyrus-sasl" ,cyrus-sasl)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("python2-dbus" ,python2-dbus)
       ("libidn" ,libidn)
       ("libltdl" ,libltdl)
       ("libxml2" ,libxml2)
       ;; TODO: gstreamer: patches needed to support gstreamer-1.0 or later
       ;; TODO: farstream
       ;; TODO: meanwhile
       ;; TODO: gtkspell
       ;; TODO: libxephyr
       ;; TODO: libgadu
       ("libxslt" ,libxslt)
       ("avahi" ,avahi)
       ("ncurses" ,ncurses)
       ("network-manager" ,network-manager)
       ("sqlite" ,sqlite)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("libxscrnsaver" ,libxscrnsaver)
       ("startup-notification" ,startup-notification)))
    (arguments
     `(#:configure-flags
       (list "--disable-gtkspell"
             "--disable-tcl"
             "--disable-meanwhile"
             "--disable-vv"  ; XXX remove when we have farstream and gstreamer
             "--disable-gstreamer" ; XXX patches needed to support gstreamer-1.0
             "--enable-cyrus-sasl"
             (string-append "--with-ncurses-headers="
                            (assoc-ref %build-inputs "ncurses")
                            "/include"))))
    (native-search-paths
     (list (search-path-specification
            (variable "PURPLE_PLUGIN_PATH")
            (files (list (string-append "lib/purple-"
                                        (version-major version))
                         "lib/pidgin")))))
    (home-page "https://www.pidgin.im/")
    (synopsis "Graphical multi-protocol instant messaging client")
    (description
     "Pidgin is a modular instant messaging client that supports many popular
chat protocols.")
    (license
     (list
      license:gpl2+    ; Most of the code
      license:lgpl2.1  ; GG protocol plugin (libpurple/protocols/gg/lib)
      license:lgpl2.0+ ; OSCAR protocol plugin (libpurple/protocols/oscar)
      ;; The following licenses cover the zephyr protocol plugin:
      (license:non-copyleft
       "file://libpurple/protocols/zephyr/mit-copyright.h"
       "See libpurple/protocols/zephyr/mit-copyright.h in the distribution.")
      (license:non-copyleft
       "file://libpurple/protocols/zephyr/mit-sipb-copyright.h"
       "See libpurple/protocols/zephyr/mit-sipb-copyright.h in the distribution.")))))

(define-public pidgin-otr
  (package
    (name "pidgin-otr")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://otr.cypherpunks.ca/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i5s9rrgbyss9rszq6c6y53hwqyw1k86s40cpsfx5ccl9bprxdgl"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("pidgin" ,pidgin)
       ("libotr" ,libotr)
       ("libgpg-error" ,libgpg-error)
       ("libgcrypt" ,libgcrypt)
       ("glib" ,glib)
       ("gtk+" ,gtk+-2)))
    (home-page "https://otr.cypherpunks.ca/")
    (synopsis "Off-the-Record Messaging plugin for Pidgin")
    (description
     "Pidgin-OTR is a plugin that adds support for OTR to the Pidgin instant
messaging client.  OTR (Off-the-Record) Messaging allows you to have private
conversations over instant messaging by providing: (1) Encryption: No one else
can read your instant messages.  (2) Authentication: You are assured the
correspondent is who you think it is.  (3) Deniability: The messages you send
do not have digital signatures that are checkable by a third party.  Anyone
can forge messages after a conversation to make them look like they came from
you.  However, during a conversation, your correspondent is assured the
messages he sees are authentic and unmodified.  (4) Perfect forward secrecy:
If you lose control of your private keys, no previous conversation is
compromised.")
    (license license:gpl2)))

(define-public znc
  (package
    (name "znc")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://znc.in/releases/archive/znc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0g8i5hsl4kinpz1wp0a2zniidv3w2sd6awq8676fds516wcc6k0y"))))
    ;; TODO: autotools support has been deprecated, and new features like i18n
    ;; are only supported when building with cmake.
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "googletest")
             (copy-recursively (assoc-ref inputs "googletest-source")
                               "googletest")
             #t)))
       #:configure-flags '("--enable-python"
                           "--enable-perl"
                           "--enable-cyrus"
                           "--with-gmock=googletest/googlemock"
                           "--with-gtest=googletest/googletest")
       #:test-target "test"))
    (native-inputs
     `(("googletest-source"
        ;; ZNC 1.7 needs a newer, unreleased googletest (a release is planned
        ;; <https://github.com/google/googletest/issues/1583>, so don't update
        ;; the public GOOGLETEST to an unstable version).  The commit is taken
        ;; from ‘third_party/googletest’ in the ZNC git repository.
        ,(let ((commit "9737e63c69e94ac5777caa0bc77c77d5206467f3"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/googletest")
                   (commit commit)))
             (file-name (git-file-name "googletest-for-znc" commit))
             (sha256
              (base32
               "0ya36n8d62zbxk6p22yffgx43mqhx2fz41gqqwbpdshjryf3wvxj")))))
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("python" ,python)))
    (inputs
     `(("openssl" ,openssl)
       ("zlib" ,zlib)
       ("icu4c" ,icu4c)
       ("cyrus-sasl" ,cyrus-sasl)))
    (home-page "https://znc.in")
    (synopsis "IRC network bouncer")
    (description "ZNC is an @dfn{IRC network bouncer} or @dfn{BNC}.  It can
detach the client from the actual IRC server, and also from selected channels.
Multiple clients from different locations can connect to a single ZNC account
simultaneously and therefore appear under the same nickname on IRC.")
    (license license:asl2.0)))

(define-public python-nbxmpp
  (package
    (name "python-nbxmpp")
    (version "0.6.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbxmpp" version))
       (sha256
        (base32
         "1vq89nhamciblyi5579bppnm4sb0zk5cg5hdipfpz174fxvl2wyd"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests
    (home-page "https://dev.gajim.org/gajim/python-nbxmpp")
    (synopsis "Non-blocking Jabber/XMPP module")
    (description
     "The goal of this python library is to provide a way for Python
applications to use Jabber/XMPP networks in a non-blocking way.  This library
was initially a fork of xmpppy, but uses non-blocking sockets.")
    (license license:gpl3+)))

(define-public python2-nbxmpp
  (package-with-python2 python-nbxmpp))

(define-public gajim
  (package
    (name "gajim")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gajim.org/downloads/"
                                  (version-major+minor version)
                                  "/gajim-" version ".tar.bz2"))
              (sha256
               (base32
                "0bzxwcpdd4ydh6d6mzpr0gxwhcb0x9ympk55fpvm1hcw9d28a716"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-plugin-dirs
           (lambda _
             (substitute* "gajim/common/configpaths.py"
               (("_paths\\['PLUGINS_USER'\\]\\]")
                "_paths['PLUGINS_USER']] + \
([os.getenv('GAJIM_PLUGIN_PATH')] \
if os.getenv('GAJIM_PLUGIN_PATH') \
and Path(os.getenv('GAJIM_PLUGIN_PATH')).is_dir() \
else [])"))
             #t))
         (replace 'check
           (lambda _
             (invoke "python" "./setup.py" "test" "-s" "test.no_gui")))
         (add-after 'install 'wrap-gi-typelib-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (name)
                  (let ((file (string-append out "/bin/" name))
                        (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                    (wrap-program file
                      `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))
                '("gajim" "gajim-remote" "gajim-history-manager")))
             #t))
         (add-after 'install 'install-icons
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (adwaita (string-append
                              (assoc-ref inputs "adwaita-icon-theme")
                              "/share/icons/Adwaita"))
                    (hicolor (string-append
                              (assoc-ref inputs "hicolor-icon-theme")
                              "/share/icons/hicolor"))
                    (icons (string-append
                            out "/lib/python"
                            ,(version-major+minor (package-version python))
                            "/site-packages/gajim/data/icons")))
               (with-directory-excursion icons
                 (symlink adwaita "Adwaita")
                 (copy-recursively hicolor "hicolor")))
             #t))
         (add-after 'install-icons 'wrap-gsettings-schema-dir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/gajim")
               ;; For GtkFileChooserDialog.
               `("GSETTINGS_SCHEMA_DIR" =
                 (,(string-append (assoc-ref inputs "gtk+")
                                  "/share/glib-2.0/schemas"))))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "GAJIM_PLUGIN_PATH")
            (separator #f)              ;single entry
            (files '("share/gajim/plugins")))
           ;; Gajim needs to use the propagated inputs of its plugins.
           (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append
                          "lib/python"
                          (version-major+minor (package-version python))
                          "/site-packages"))))))
    (native-inputs
     `(("intltool" ,intltool)
       ("python-docutils" ,python-docutils)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("adwaita-icon-theme" ,adwaita-icon-theme)
       ("gnome-keyring" ,gnome-keyring)
       ("gtk+" ,gtk+)
       ("gtkspell3" ,gtkspell3)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("libsecret" ,libsecret)
       ("python-cssutils" ,python-cssutils)
       ("python-dbus" ,python-dbus)
       ("python-gnupg" ,python-gnupg)
       ("python-keyring" ,python-keyring)
       ("python-nbxmpp" ,python-nbxmpp)
       ("python-pillow" ,python-pillow)
       ("python-precis-i18n" ,python-precis-i18n)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pyopenssl" ,python-pyopenssl)
       ("python-qrcode" ,python-qrcode)))
    (home-page "https://gajim.org/")
    (synopsis "Jabber (XMPP) client")
    (description "Gajim is a feature-rich and easy to use Jabber/XMPP client.
Among its features are: a tabbed chat window and single window modes; support
for group chat (with Multi-User Chat protocol), invitation, chat to group chat
transformation; audio and video conferences; file transfer; TLS, GPG and
end-to-end encryption support; XML console.")
    (license license:gpl3)))

(define-public gajim-omemo
  (package
    (name "gajim-omemo")
    (version "2.6.29")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://ftp.gajim.org/plugins_releases/omemo_"
                    version ".zip"))
              (sha256
               (base32
                "1mif5qkrvxclqbqmq6njini4laznbs5nn82w2f1hkl8c1284dvgi"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (in-vicinity out "share/gajim/plugins"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p share)
           (copy-recursively source share)
           #t))))
    (propagated-inputs
     `(("python-axolotl" ,python-axolotl)))
    (home-page
     "https://dev.gajim.org/gajim/gajim-plugins/wikis/OmemoGajimPlugin")
    (synopsis "Gajim OMEMO plugin")
    (description
     "This package provides the Gajim OMEMO plugin.  OMEMO is an XMPP
Extension Protocol (XEP) for secure multi-client end-to-end encryption based
on Axolotl and PEP.")
    (license license:gpl3+)))

(define-public dino
  ;; The only release tarball is for version 0.0, but it is very old and fails
  ;; to build.
  (let ((commit "8e14ac6d714b7f88e16de31a6c795e811dc27417")
        (revision "4"))
    (package
      (name "dino")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dino/dino.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xfmwnc2f8lsvmp7m8ggikzqjaw5z6wmxrv6j5ljha5ckffrdd9m"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ; there are no tests
         #:parallel-build? #f ; not supported
         ; Use our libsignal-protocol-c instead of the git submodule.
         #:configure-flags '("-DSHARED_SIGNAL_PROTOCOL=yes")
         #:modules ((guix build cmake-build-system)
                    ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                    (guix build utils))
         #:imported-modules (,@%gnu-build-system-modules
                             (guix build cmake-build-system)
                             (guix build glib-or-gtk-build-system))
         #:phases
         (modify-phases %standard-phases
           ;; The signal-protocol plugin accesses internal headers of
           ;; libsignal-protocol-c, so we need to put the sources there.
           (add-after 'unpack 'unpack-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (with-directory-excursion "plugins/signal-protocol/libsignal-protocol-c"
                 (invoke "tar" "xvf"
                         (assoc-ref inputs "libsignal-protocol-c-source")
                         "--strip-components=1"))))
           (add-after 'install 'glib-or-gtk-wrap
             (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
      (inputs
       `(("libgee" ,libgee)
         ("libsignal-protocol-c" ,libsignal-protocol-c)
         ("libgcrypt" ,libgcrypt)
         ("libsoup" ,libsoup)
         ("qrencode" ,qrencode)
         ("sqlite" ,sqlite-with-column-metadata)
         ("gpgme" ,gpgme)
         ("gtk+" ,gtk+)
         ("glib-networking" ,glib-networking)
         ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("libsignal-protocol-c-source" ,(package-source libsignal-protocol-c))
         ("glib" ,glib "bin")
         ("vala" ,vala)
         ("gettext" ,gettext-minimal)))
      (home-page "https://dino.im")
      (synopsis "Graphical Jabber (XMPP) client")
      (description "Dino is a Jabber (XMPP) client which aims to fit well into
a graphical desktop environment like GNOME.")
      (license license:gpl3+))))

(define-public prosody
  (package
    (name "prosody")
    (version "0.11.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://prosody.im/downloads/source/"
                                  "prosody-" version ".tar.gz"))
              (sha256
               (base32
                "0ca8ivqb4hxqka08pwnaqi1bqxrdl8zw47g6z7nw9q5r57fgc4c9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;tests require "busted"
       #:configure-flags (list "--no-example-certs")
       #:modules ((ice-9 match)
                  (srfi srfi-1)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configure-script
           (lambda _
             ;; The configure script aborts when it encounters unexpected
             ;; arguments.  Make it more tolerant.
             (substitute* "configure"
               (("exit 1") ""))
             #t))
         (add-after 'unpack 'fix-makefile
           (lambda _
             (substitute* "GNUmakefile"
               ;; prosodyctl needs to read the configuration file.
               (("^INSTALLEDCONFIG =.*") "INSTALLEDCONFIG = /etc/prosody\n")
               ;; prosodyctl needs a place to put auto-generated certificates.
               (("^INSTALLEDDATA =.*") "INSTALLEDDATA = /var/lib/prosody\n"))
             #t))
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure all executables in "bin" find the required Lua
             ;; modules at runtime.
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin/"))
                    (deps  (delete #f (map (match-lambda
                                             ((label . directory)
                                              (if (string-prefix? "lua" label)
                                                  directory #f)))
                                           inputs)))
                    (lua-path (string-join
                               (map (lambda (path)
                                      (string-append
                                       path "/share/lua/5.2/?.lua;"
                                       path "/share/lua/5.2/?/?.lua"))
                                    (cons out deps))
                               ";"))
                    (lua-cpath (string-join
                                (map (lambda (path)
                                       (string-append
                                        path "/lib/lua/5.2/?.so;"
                                        path "/lib/lua/5.2/?/?.so"))
                                     (cons out deps))
                                ";"))
                    (openssl (assoc-ref inputs "openssl"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (path (map (lambda (dir)
                                 (string-append dir "/bin"))
                               (list openssl coreutils))))
               (for-each (lambda (file)
                           (wrap-program file
                             `("LUA_PATH"  ";" = (,lua-path))
                             `("LUA_CPATH" ";" = (,lua-cpath))
                             `("PATH" ":" prefix ,path)))
                         (find-files bin ".*"))
               #t))))))
    (inputs
     `(("libidn" ,libidn)
       ("openssl" ,openssl)
       ("lua" ,lua-5.2)
       ("lua5.2-bitop" ,lua5.2-bitop)
       ("lua5.2-expat" ,lua5.2-expat)
       ("lua5.2-socket" ,lua5.2-socket)
       ("lua5.2-filesystem" ,lua5.2-filesystem)
       ("lua5.2-sec" ,lua5.2-sec)))
    (home-page "https://prosody.im/")
    (synopsis "Jabber (XMPP) server")
    (description "Prosody is a modern XMPP communication server.  It aims to
be easy to set up and configure, and efficient with system resources.
Additionally, for developers it aims to be easy to extend and give a flexible
system on which to rapidly develop added functionality, or prototype new
protocols.")
    (license license:x11)))

(define-public prosody-http-upload
  (let ((changeset "765735bb590b")
        (revision "1"))
    (package
      (name "prosody-http-upload")
      (version (string-append "0-" revision "." (string-take changeset 7)))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://hg.prosody.im/prosody-modules/")
                      (changeset changeset)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "142wrcism70nf8ffahhd961cqg2pi1h7ic8adfs3zwh0j3pnf41f"))))
      (build-system trivial-build-system)
      (arguments
       '(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((out (assoc-ref %outputs "out"))
                 (source (assoc-ref %build-inputs "source")))
             (with-directory-excursion (in-vicinity source "mod_http_upload")
               (install-file "mod_http_upload.lua" out))
             #t))))
      (home-page "https://modules.prosody.im/mod_http_upload.html")
      (synopsis "XEP-0363: Allow clients to upload files over HTTP")
      (description "This module implements XEP-0363: it allows clients to
upload files over HTTP.")
      (license (package-license prosody)))))

(define-public prosody-smacks
  (let ((changeset "67f1d1f22625")
        (revision "1"))
    (package
      (name "prosody-smacks")
      (version (string-append "0-" revision "." (string-take changeset 7)))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://hg.prosody.im/prosody-modules/")
                      (changeset changeset)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "020ngpax30fgarah98yvlj0ni8rcdwq60if03a9hqdw8mic0nxxs"))))
      (build-system trivial-build-system)
      (arguments
       '(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((out (assoc-ref %outputs "out"))
                 (source (assoc-ref %build-inputs "source")))
             (with-directory-excursion (in-vicinity source "mod_smacks")
               (install-file "mod_smacks.lua" out))
             #t))))
      (home-page "https://modules.prosody.im/mod_smacks.html")
      (synopsis "XEP-0198: Reliability and fast reconnects for XMPP")
      (description "This module implements XEP-0198: when supported by both
the client and server, it can allow clients to resume a disconnected session,
and prevent message loss.")
      (license (package-license prosody)))))

(define-public libtoxcore
  (let ((revision "2")
        (commit "bf69b54f64003d160d759068f4816b2d9b2e1e21"))
    (package
      (name "libtoxcore")
      (version (string-append "0.0.0" "-"
                              revision "."(string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/irungentoo/toxcore.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "11lqra4yd7v293cp286ynff5lqz1pprzg8vn3wq6vryj08g88zqb"))))
      (build-system gnu-build-system)
      (arguments `(#:tests? #f)) ; FIXME: tests hang, some fail.
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("check" ,check)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("libsodium" ,libsodium)
         ("opus" ,opus)
         ("libvpx" ,libvpx)))
      (synopsis "Library for the Tox encrypted messenger protocol")
      (description
       "C library implementation of the Tox encrypted messenger protocol.")
      (license license:gpl3+)
      (home-page "https://tox.chat"))))

;; Some tox clients move to c-toxcore, which seems to be where all the
;; recent development happens. It is run by the same developers as toxcore,
;; forked into a group namespace.
(define-public c-toxcore
  (package
    (name "c-toxcore")
    (version "0.2.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TokTok/c-toxcore.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0aljr9hqybla6p61af6fdkv0x8gph7c2wacqqa9hq2z9w0p4fs5j"))))
    (arguments
     `(#:tests? #f)) ; FIXME: Testsuite seems to stay stuck on test 3. Disable
                     ; for now.
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libsodium" ,libsodium)
       ("opus" ,opus)
       ("libvpx" ,libvpx)))
    (home-page "https://tox.chat")
    (synopsis "Library for the Tox encrypted messenger protocol")
    (description
     "Official fork of the C library implementation of the Tox encrypted
messenger protocol.")
    (license license:gpl3+)))

(define-public utox
  (package
   (name "utox")
   (version "0.17.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/uTox/uTox.git")
           (commit "v0.17.0")
           (recursive? #t))) ;; Needed for 'minini' git submodule.
     (file-name (string-append name "-" version "-checkout"))
     (sha256
      (base32
       "12wbq883il7ikldayh8hm0cjfrkp45vn05xx9s1jbfz6gmkidyar"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags '("-DENABLE_TESTS=on")
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'patch-absolute-filename-libgtk-3
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "../source/src/xlib/gtk.c"
                         (("libgtk-3.so")
                         (string-append (assoc-ref inputs "gtk+")
                                        "/lib/libgtk-3.so")))))
        (add-after 'install 'wrap-program
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (wrap-program (string-append (assoc-ref outputs "out")
                                         "/bin/utox")
            ;; For GtkFileChooserDialog.
            `("GSETTINGS_SCHEMA_DIR" =
              (,(string-append (assoc-ref inputs "gtk+")
                               "/share/glib-2.0/schemas")))))))))
   (inputs
    `(("dbus" ,dbus)
      ("filteraudio" ,filteraudio)
      ("fontconfig" ,fontconfig)
      ("freetype" ,freetype)
      ("libsodium" ,libsodium)
      ("c-toxcore" ,c-toxcore)
      ("gtk+" ,gtk+)
      ("libvpx" ,libvpx)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxrender" ,libxrender)
      ("openal" ,openal)
      ("v4l-utils" ,v4l-utils)))
   (native-inputs
    `(("check" ,check)
      ("pkg-config" ,pkg-config)))
   (synopsis "Lightweight Tox client")
   (description
    "Utox is a lightweight Tox client.  Tox is a distributed and secure
instant messenger with audio and video chat capabilities.")
   (home-page "http://utox.org/")
   (license license:gpl3)))

(define-public qtox
  (package
    (name "qtox")
    (version "1.16.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/qTox/qTox/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10n3cgw9xaqin9la8wpd8v83bkjmimicgbyp5ninsdgsrgky4hmq"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-reproducibility-issues
           (lambda _
             (substitute* "src/main.cpp"
               (("__DATE__") "\"\"")
               (("__TIME__") "\"\"")
               (("TIMESTAMP") "\"\""))
             #t))
         ;; Ensure that icons are found at runtime.
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/qtox")
                 `("QT_PLUGIN_PATH" prefix
                   ,(list (string-append (assoc-ref inputs "qtsvg")
                                         "/lib/qt5/plugins/"))))))))))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("filteraudio" ,filteraudio)
       ("glib" ,glib)
       ("gtk+" ,gtk+-2)
       ("libsodium" ,libsodium)
       ("c-toxcore" ,c-toxcore)
       ("libvpx" ,libvpx)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libx11" ,libx11)
       ("libexif" ,libexif)
       ("sqlite" ,sqlite)
       ("openal" ,openal)
       ("qrencode" ,qrencode)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("sqlcipher" ,sqlcipher)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qmake" ,qttools)))
    (home-page "https://qtox.github.io/")
    (synopsis "Tox chat client using Qt")
    (description "qTox is a Tox client that follows the Tox design
guidelines.  It provides an easy to use application that allows you to
connect with friends and family without anyone else listening in.")
    (license license:gpl3+)))

(define-public pybitmessage
  (package
    (name "pybitmessage")
    (version "0.6.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Bitmessage/PyBitmessage.git")
             (commit version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1lmhbpwsqh1v93krlqqhafw2pc3y0qp8zby186yllbph6s8kdp35"))))
    (propagated-inputs
     ;; TODO:
     ;; Package "pyopencl", required in addition to numpy for OpenCL support.
     ;; Package "gst123", required in addition to alsa-utils and
     ;; mpg123 for sound support.
     `(("python2-msgpack" ,python2-msgpack)
       ("python2-pythondialog" ,python2-pythondialog)
       ("python2-pyqt-4" ,python2-pyqt-4)
       ("python2-sip" ,python2-sip)
       ("python2-pysqlite" ,python2-pysqlite)
       ("python2-pyopenssl" ,python2-pyopenssl)))
    (native-inputs
     `(("openssl" ,openssl)))
    (build-system python-build-system)
    (arguments
     `(#:modules ((guix build python-build-system)
                  (guix build utils))
       #:tests? #f ;no test target
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-unmatched-python-shebangs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/bitmessagemain.py"
               (("#!/usr/bin/env python2.7")
                (string-append "#!" (which "python"))))
             (substitute* "src/bitmessagecli.py"
               (("#!/usr/bin/env python2.7.x")
                (string-append "#!" (which "python"))))
             #t))
         (add-after 'unpack 'fix-depends
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/depends.py"
               (("libcrypto.so")
                (string-append (assoc-ref inputs "openssl")
                               "/lib/libcrypto.so")))
             #t))
         (add-after 'unpack 'fix-local-files-in-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/proofofwork.py"
               (("bitmsghash.so")
                (string-append (assoc-ref outputs "out")
                               "/lib/bitmsghash.so")))
             #t))
         (add-after 'unpack 'fix-pyelliptic
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/pyelliptic/openssl.py"
               (("libcrypto.so")
                (string-append (assoc-ref inputs "openssl")
                               "/lib/libcrypto.so"))
               (("libssl.so")
                (string-append (assoc-ref inputs "openssl")
                               "/lib/libssl.so")))
             #t))
         (add-after 'unpack 'noninteractive-build
           ;; This applies upstream commit 4c597d3f7cf9f83a763472aa165a1a4292019f20
           (lambda _
             (substitute* "setup.py"
               (("except NameError")
                "except EOFError, NameError"))
             #t))
         ;; XXX: python setup.py does not build and install bitmsghash,
         ;; without it PyBitmessage tries to compile it at first run
         ;; in the store, which due to obvious reasons fails. Do it
         ;; and place it in /lib.
         (add-after 'unpack 'build-and-install-bitmsghash
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "src/bitmsghash"
               (system* "make")
               (install-file "bitmsghash.so"
                             (string-append (assoc-ref outputs "out") "/lib")))
             #t)))))
    (license license:expat)
    (description
     "Distributed and trustless peer-to-peer communications protocol
for sending encrypted messages to one person or many subscribers.")
    (synopsis "Distributed peer-to-peer communication")
    (home-page "https://bitmessage.org/")))

(define-public ytalk
  (package
    (name "ytalk")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.ourproject.org/pub/ytalk/ytalk-"
                           version  ".tar.gz"))
       (sha256
        (base32
         "1d3jhnj8rgzxyxjwfa22vh45qwzjvxw1qh8fz6b7nfkj3zvk9jvf"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "http://ytalk.ourproject.org")
    (synopsis "Multi-user chat program")
    (description "Ytalk is a replacement for the BSD talk program.  Its main
advantage is the ability to communicate with any arbitrary number of users at
once.  It supports both talk protocols (\"talk\" and \"ntalk\") and can communicate
with several different talk daemons at the same time.")
    (license license:gpl2+)))

(define-public gloox
  (package
    (name "gloox")
    (version "1.0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://camaya.net/download/gloox-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0r69gq8if9yy1amjzl7qrq9lzhhna7qgz905ln4wvkwchha1ppja"))))
    (build-system gnu-build-system)
    (inputs
     `(("libidn" ,libidn)
       ("gnutls" ,gnutls)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Portable high-level Jabber/XMPP library for C++")
    (description
     "gloox is a full-featured Jabber/XMPP client library,
written in ANSI C++.  It makes writing spec-compliant clients easy
and allows for hassle-free integration of Jabber/XMPP functionality
into existing applications.")
    (home-page "https://camaya.net/gloox")
    (license license:gpl3)))

(define-public perl-net-psyc
  (package
    (name "perl-net-psyc")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://perlpsyc.psyc.eu/"
                           "perlpsyc-" version ".zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "0vsjclglkwgbyd9m5ad642fyysxw2x725nhq4r2m9pvqaq6s5yf2"))))
    (build-system perl-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("perl-curses" ,perl-curses)
       ("perl-io-socket-ssl" ,perl-io-socket-ssl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configure script
         ;; There is a Makefile, but it does not install everything
         ;; (leaves out psycion) and says
         ;; "# Just to give you a rough idea". XXX: Fix it upstream.
         (replace 'build
           (lambda _ (invoke "make" "manuals")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/perl-net-psyc"))
                    (man1 (string-append out "/share/man/man1"))
                    (man3 (string-append out "/share/man/man3"))
                    (bin (string-append out "/bin"))
                    (libpsyc (string-append out "/lib/psyc/ion"))
                    (libperl (string-append out "/lib/perl5/site_perl/"
                                            ,(package-version perl))))

               (copy-recursively "lib/perl5" libperl)
               (copy-recursively "lib/psycion" libpsyc)
               (copy-recursively "bin" bin)
               (install-file "cgi/psycpager" (string-append doc "/cgi"))
               (copy-recursively "contrib" (string-append doc "/contrib"))
               (copy-recursively "hooks" (string-append doc "/hooks"))
               (copy-recursively "sdj" (string-append doc "/sdj"))
               (install-file "README.txt" doc)
               (install-file "TODO.txt" doc)
               (copy-recursively "share/man/man1" man1)
               (copy-recursively "share/man/man3" man3)
               #t)))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure all executables in "bin" find the Perl modules
             ;; provided by this package at runtime.
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin/"))
                    (path (getenv "PERL5LIB")))
               (for-each (lambda (file)
                           (wrap-program file
                             `("PERL5LIB" ":" prefix (,path))))
                         (find-files bin "\\.*$"))
               #t))))))
    (description
     "@code{Net::PSYC} with support for TCP, UDP, Event.pm, @code{IO::Select} and
Gtk2 event loops.  This package includes 12 applications and additional scripts:
psycion (a @uref{http://about.psyc.eu,PSYC} chat client), remotor (a control console
for @uref{https://torproject.org,tor} router) and many more.")
    (synopsis "Perl implementation of PSYC protocol")
    (home-page "http://perlpsyc.psyc.eu/")
    (license (list license:gpl2
                   license:perl-license
                   ;; contrib/irssi-psyc.pl:
                   license:public-domain
                   ;; bin/psycplay states AGPL with no version:
                   license:agpl3+))))

(define-public libpsyc
  (package
    (name "libpsyc")
    (version "20160913")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.psyced.org/files/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "14q89fxap05ajkfn20rnhc6b1h4i3i2adyr7y6hs5zqwb2lcmc1p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("netcat" ,netcat)
       ("procps" ,procps)))
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; The rust bindings are the only ones in use, the lpc bindings
         ;; are in psyclpc.  The other bindings are not used by anything,
         ;; the chances are high that the bindings do not even work,
         ;; therefore we do not include them.
         ;; TODO: Get a cargo build system in Guix.
         (delete 'configure)))) ; no configure script
    (home-page "http://about.psyc.eu/libpsyc")
    (description
     "@code{libpsyc} is a PSYC library in C which implements
core aspects of PSYC, useful for all kinds of clients and servers
including psyced.")
    (synopsis "PSYC library in C")
    (license license:agpl3+)))

;; This commit removes the historic bundled pcre and makes psyclpc reproducible.
(define-public psyclpc
  (let* ((commit "61cf9aa81297085e5c40170fd01221c752f8deba")
         (revision "2"))
  (package
    (name "psyclpc")
    (version (string-append "20160821-" revision "." (string-take commit 7)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.psyced.org/git/psyclpc")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1viwqymbhn3cwvx0zl58rlzl5gw47zxn0ldg2nbi55ghm5zxl1z5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests/checks.
       #:configure-flags
       ;; If you have questions about this part, look at
       ;; "src/settings/psyced" and the ebuild.
       (list
        "--enable-use-tls=yes"
        "--enable-use-mccp" ; Mud Client Compression Protocol, leave this enabled.
        (string-append "--prefix="
                       (assoc-ref %outputs "out"))
        ;; src/Makefile: Set MUD_LIB to the directory which contains
        ;; the mud data. defaults to MUD_LIB = @libdir@
        (string-append "--libdir="
                       (assoc-ref %outputs "out")
                       "/opt/psyced/world")
        (string-append "--bindir="
                       (assoc-ref %outputs "out")
                       "/opt/psyced/bin")
        ;; src/Makefile: Set ERQ_DIR to directory which contains the
        ;; stuff which ERQ can execute (hopefully) savely.  Was formerly
        ;; defined in config.h. defaults to ERQ_DIR= @libexecdir@
        (string-append "--libexecdir="
                       (assoc-ref %outputs "out")
                       "/opt/psyced/run"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir-to-src
           ;; We need to pass this as env variables
           ;; and manually change the directory.
           (lambda _
             (chdir "src")
             (setenv "CONFIG_SHELL" (which "sh"))
             (setenv "SHELL" (which "sh"))
             #t)))
       #:make-flags (list "install-all")))
    (inputs
     `(("zlib" ,zlib)
       ("openssl" ,openssl)
       ("pcre" ,pcre)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("gettext" ,gettext-minimal)
       ("help2man" ,help2man)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "http://lpc.psyc.eu/")
    (synopsis "psycLPC is a multi-user network server programming language")
    (description
     "LPC is a bytecode language, invented to specifically implement
multi user virtual environments on the internet.  This technology is used for
MUDs and also the psyced implementation of the Protocol for SYnchronous
Conferencing (PSYC).  psycLPC is a fork of LDMud with some new features and
many bug fixes.")
    (license license:gpl2))))

(define-public loudmouth
  (package
    (name "loudmouth")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mcabber.com/files/loudmouth/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "0b6kd5gpndl9nzis3n6hcl0ldz74bnbiypqgqa1vgb0vrcar8cjl"))))
    (build-system gnu-build-system)
    (inputs
     `(("glib" ,glib)
       ("gnutls" ,gnutls)
       ("libidn" ,libidn)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("check" ,check)
       ("glib" ,glib "bin") ; gtester
       ("gtk-doc" ,gtk-doc)))
    (home-page "https://mcabber.com/")
    (description
     "Loudmouth is a lightweight and easy-to-use C library for programming
with the XMPP (formerly known as Jabber) protocol.  It is designed to be
easy to get started with and yet extensible to let you do anything the XMPP
protocol allows.")
    (synopsis "Asynchronous XMPP library")
    ;; The files have LGPL2.0+ headers, but COPYING specifies LGPL2.1.
    (license license:lgpl2.0+)))

(define-public mcabber
  (package
    (name "mcabber")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mcabber.com/files/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "1ggh865p1rf10ffsnf4g6qv9i8bls36dxdb1nzs5r9vdqci2rz04"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list "--enable-otr"
                               "--enable-aspell")))
    (inputs
     `(("gpgme" ,gpgme)
       ("libotr" ,libotr)
       ("aspell" ,aspell)
       ("libidn" ,libidn)
       ("glib" ,glib)
       ("ncurses" ,ncurses)
       ("loudmouth" ,loudmouth)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://mcabber.com")
    (description
     "Mcabber is a small XMPP (Jabber) console client, which includes features
such as SASL and TLS support, @dfn{Multi-User Chat} (MUC) support, logging,
command-completion, OpenPGP encryption, @dfn{Off-the-Record Messaging} (OTR)
support, and more.")
    (synopsis "Small XMPP console client")
    (license license:gpl2+)))

(define-public freetalk
  (package
    (name "freetalk")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freetalk/freetalk-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rmrn7a1bb7vm26yaklrvx008a9qhwc32s57dwrlf40lv9gffwny"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; For 'system' commands in Scheme code.
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (bash      (assoc-ref inputs "bash"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (less      (assoc-ref inputs "less")))
               (wrap-program (string-append out "/bin/freetalk")
                 `("PATH" ":" prefix
                   ,(map (lambda (dir)
                           (string-append dir "/bin"))
                         (list bash coreutils less))))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (inputs
     `(("bash" ,bash)
       ("glib" ,glib)
       ("guile" ,guile-2.0)
       ("less" ,less)
       ("loudmouth" ,loudmouth)
       ("readline" ,readline)))
    (synopsis "Extensible console-based Jabber client")
    (description
     "GNU Freetalk is a command-line Jabber/XMPP chat client.  It notably uses
the Readline library to handle input, so it features convenient navigation of
text as well as tab-completion of buddy names, commands and English words.  It
is also scriptable and extensible via Guile.")
    (home-page "https://www.gnu.org/software/freetalk/")
    (license license:gpl3+)))

(define-public libmesode
  (package
    (name "libmesode")
    (version "0.9.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/boothj5/libmesode.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06f5nfaypvxrbsinxa1k2vrxrs7kqmg38g4wwwk5d63hpn1pj8ak"))))
    (build-system gnu-build-system)
    (inputs
     `(("expat" ,expat)
       ("openssl" ,openssl)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (synopsis "C library for writing XMPP clients")
    (description "Libmesode is a fork of libstrophe for use with Profanity
XMPP Client.  In particular, libmesode provides extra TLS functionality such as
manual SSL certificate verification.")
    (home-page "https://github.com/boothj5/libmesode")
    ;; Dual licensed.
    (license (list license:gpl3+ license:x11))))

(define-public libstrophe
  (package
    (name "libstrophe")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/strophe/libstrophe/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vxfcyfnhnlaj6spm2b0ljw5i3knbphy6mvzpl5zv9b52ny4b08m"))))
    (build-system gnu-build-system)
    (inputs
     `(("expat" ,expat)
       ("openssl" ,openssl)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (synopsis "C library for writing XMPP clients")
    (description "Libstrophe is a minimal XMPP library written in C.  It has
almost no external dependencies, only an XML parsing library (expat or libxml
are both supported).")
    (home-page "http://strophe.im/libstrophe")
    ;; Dual licensed.
    (license (list license:gpl3+ license:x11))))

(define-public profanity
    (package
        (name "profanity")
        (version "0.5.1")
        (source (origin
                  (method url-fetch)
                  (uri (string-append "http://www.profanity.im/profanity-"
                                      version ".tar.gz"))
                  (sha256
                   (base32
                     "1f7ylw3mhhnii52mmk40hyc4kqhpvjdr3hmsplzkdhsfww9kflg3"))))
        (build-system gnu-build-system)
        (inputs
         `(("curl" ,curl)
           ("expat" ,expat)
           ("glib" ,glib)
           ("gpgme" ,gpgme)
           ("libmesode" ,libmesode)
           ("libotr" ,libotr)
           ("ncurses" ,ncurses)
           ("openssl" ,openssl)
           ("readline" ,readline)))
        (native-inputs
         `(("autoconf" ,autoconf)
           ("autoconf-archive" ,autoconf-archive)
           ("automake" ,automake)
           ("cmocka" ,cmocka)
           ("libtool" ,libtool)
           ("pkg-config" ,pkg-config)))
        (synopsis "Console-based XMPP client")
        (description "Profanity is a console based XMPP client written in C
using ncurses and libmesode, inspired by Irssi.")
        (home-page "http://www.profanity.im")
        (license license:gpl3+)))

(define-public libircclient
  (package
    (name "libircclient")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/libircclient/libircclient/"
                           version "/libircclient-" version ".tar.gz"))
       (sha256
        (base32
         "0b9wa0h3xc31wpqlvgxgnvqp5wgx3kwsf5s9432m5cj8ycx6zcmv"))))
    (build-system gnu-build-system)
    (inputs
     `(("openssl" ,openssl)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir="
                            (assoc-ref %outputs "out") "/lib")
             "--enable-shared"
             "--enable-ipv6"
             "--enable-openssl")
       #:tests? #f))                    ; no test suite
    (home-page "https://www.ulduzsoft.com/libircclient/")
    (synopsis "Library implementing the client IRC protocol")
    (description "Libircclient is a library which implements the client IRC
protocol.  It is designed to be small, fast, portable and compatible with the
RFC standards as well as non-standard but popular features.  It can be used for
building the IRC clients and bots.")
    (license license:lgpl3+)))

(define-public toxic
  (package
    (name "toxic")
    (version "0.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JFreegman/toxic.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "09l2j3lwvrq7bf3051vjsnml9w63790ly3iylgf26gkrmld6k31w"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        "CC=gcc"
        (string-append "PREFIX="
                       (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'enable-python-scripting
           (lambda _
             (setenv "ENABLE_PYTHON" "1")
             #t)))))
    (inputs
     `(("c-toxcore" ,c-toxcore)
       ("curl" ,curl)
       ("freealut" ,freealut)
       ("gdk-pixbuf" ,gdk-pixbuf)       ; for libnotify.pc
       ("libconfig" ,libconfig)
       ("libnotify" ,libnotify)
       ("libpng" ,libpng)
       ("libvpx" ,libvpx)
       ("libx11" ,libx11)
       ("ncurses" ,ncurses)
       ("openal" ,openal)
       ("python" ,python)
       ("qrencode" ,qrencode)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/JFreegman/toxic")
    (synopsis "Tox chat client using ncurses")
    (description "Toxic is a console-based instant messaging client, using
c-toxcore and ncurses.  It provides audio calls, sound and desktop
notifications, and Python scripting support.")
    (license license:gpl3+)))

(define-public libqmatrixclient
  (package
    (name "libqmatrixclient")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/QMatrixClient/libqmatrixclient")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1llzqjagvp91kcg26q5c4qw9aaz7wna3rh6k06rc3baivrjqf3cn"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON")
       #:tests? #f)) ; No tests
    (home-page "https://matrix.org/docs/projects/sdk/libqmatrixclient.html")
    (synopsis "Qt5 client library for the Matrix instant messaging protocol")
    (description "libqmatrixclient is a Qt5 library to write clients for the
Matrix instant messaging protocol.  Quaternion is the reference client
implementation.  Quaternion and libqmatrixclient together form the
QMatrixClient project.")
    (license license:lgpl2.1+)))

(define-public quaternion
  (package
    (name "quaternion")
    (version "0.0.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/QMatrixClient/Quaternion")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hr9zqf301rg583n9jv256vzj7y57d8qgayk7c723bfknf1s6hh3"))))
    (build-system cmake-build-system)
    (inputs
     `(("libqmatrixclient" ,libqmatrixclient)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtsvg" ,qtsvg)
       ("qttools" ,qttools)))
    (arguments
     `(#:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/quaternion")
               `("QT_PLUGIN_PATH" ":" prefix
                 (,(string-append (assoc-ref inputs "qtsvg")
                                  "/lib/qt5/plugins")))
               `("QML2_IMPORT_PATH" ":" prefix
                 ,(map (lambda (label)
                         (string-append (assoc-ref inputs label)
                                        "/lib/qt5/qml"))
                       '("qtdeclarative" "qtquickcontrols"))))
             #t)))))
    (home-page "https://matrix.org/docs/projects/client/quaternion.html")
    (synopsis "Graphical client for the Matrix instant messaging protocol")
    (description "Quaternion is a Qt5 desktop client for the Matrix instant
messaging protocol.  It uses libqmatrixclient and is its reference client
implementation.  Quaternion and libqmatriclient together form the
QMatrixClient project.")
    (license (list license:gpl3+ ; all source code
                   license:lgpl3+)))) ; icons/breeze

(define-public hangups
  (package
    (name "hangups")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hangups" version))
       (sha256
        (base32 "1jw4i58cd4j1ymsnhv9224xsi26w8y0qrj6z4nw50dnbl45b6aaa"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'relax-dependencies
           ;; Relax overly strict package version specifications.
           (lambda _
             (substitute* "setup.py"
               (("==") ">="))
             #t)))))
    (propagated-inputs
     `(("python-aiohttp" ,python-aiohttp)
       ("python-appdirs" ,python-appdirs)
       ("python-async-timeout" ,python-async-timeout)
       ("python-configargparse" ,python-configargparse)
       ("python-mechanicalsoup" ,python-mechanicalsoup)
       ("python-protobuf" ,python-protobuf)
       ("python-readlike" ,python-readlike)
       ("python-reparser" ,python-reparser)
       ("python-requests" ,python-requests)
       ("python-urwid" ,python-urwid)))
    (native-inputs
     `(("python-httpretty" ,python-httpretty)
       ("python-pytest" ,python-pytest)))
    (home-page "https://hangups.readthedocs.io/")
    (synopsis "Instant messaging client for Google Hangouts")
    (description
     "Hangups is an instant messaging client for Google Hangouts.  It includes
both a Python library and a reference client with a text-based user interface.

Hangups is implements a reverse-engineered version of Hangouts' proprietary,
non-interoperable protocol, which allows it to support features like group
messaging that aren’t available to clients that connect over XMPP.")
    (license license:expat)))

(define-public telegram-purple
  (package
    (name "telegram-purple")
    (version "1.3.1")
    (home-page "https://github.com/majn/telegram-purple")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (sha256
               (base32
                "0p93jpjpx7hszwffzgixw04zkrpsiyzz4za3gfr4j07krc4771fp"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile.in"
                    ;; By default these two directories point to Pidgin's own
                    ;; prefix.
                    (("^PLUGIN_DIR_PURPLE=.*")
                     (string-append
                      "exec_prefix := @exec_prefix@\n"
                      "PLUGIN_DIR_PURPLE := @libdir@/purple-2\n"))
                    (("^DATA_ROOT_DIR_PURPLE=.*")
                     "DATA_ROOT_DIR_PURPLE := @datarootdir@\n")

                    ;; Honor sysconfdir instead of trying to write to /etc.
                    (("DESTDIR\\)/etc/telegram-purple")
                     "DESTDIR)@sysconfdir@/telegram-purple"))
                  #t))
              (patches (search-patches "telegram-purple-adjust-test.patch"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("pidgin" ,pidgin)
       ("libgcrypt" ,libgcrypt)
       ("libwebp" ,libwebp)
       ("glib" ,glib)
       ("gettext" ,gnu-gettext)
       ("gtk+" ,gtk+-2)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; We're using release tag for repository checkout - let's prepare
         ;; header defining GIT_COMMIT manually instead of running git to
         ;; identify version which is being compiled. Git repository
         ;; is removed anyway and only source code is kept.
         (add-after 'unpack 'prepare-commit.h
           (lambda _
             (with-output-to-file "./commit.h"
               (lambda ()
                 (display
                  (string-append "//generated by guix, use version instead of "
                                 "commit\n"
                                 "#ifndef GIT_COMMIT\n"
                                 "#  define GIT_COMMIT \"v"
                                 ,version "\"\n"
                                 "#endif\n"))))
             #t))
         (add-before 'configure 'set-SHELL-variables
           ;; Set these environment variables so that 'tgl/configure' uses the
           ;; right shell and not /bin/sh.
           (lambda _
             (let ((bash (which "bash")))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               #t))))))
    (synopsis "Telegram messaging support for Pidgin")
    (description
     "Telegram-purple is a plugin for Libpurple, the communication library
used by the Pidgin instant messaging client, that adds support for the
Telegram messenger.")

    ;; Code under tgl/ (the Telegram library) is LGPLv2.1+, but the plugin
    ;; itself is GPLv2+.
    (license license:gpl2+)))

;;; messaging.scm ends here
