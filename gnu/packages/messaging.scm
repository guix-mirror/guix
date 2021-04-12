;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016, 2017, 2018, 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017, 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019, 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Mason Hock <chaosmonk@riseup.net>
;;; Copyright © 2020, 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021 Robert Karszniewicz <avoidr@posteo.de>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages less)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages mono)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages php)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sphinx)
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
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public psi
  (package
    (name "psi")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/psi/Psi/"
                       version "/psi-" version ".tar.xz"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           (delete-file-recursively "3rdparty")))
       (sha256
        (base32 "1dxmm1d1zr0pfs51lba732ipm6hm2357jlfb934lvarzsh7karri"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:configure-flags
       (list
        "-DUSE_ENCHANT=ON"
        "-DUSE_CCACHE=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-cmake
           (lambda _
             (substitute* "cmake/modules/FindHunspell.cmake"
               (("hunspell-1.6")
                "hunspell-1.7"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("ruby" ,ruby)))
    (inputs
     `(("aspell" ,aspell)
       ("enchant" ,enchant-1.6)
       ("hunspell" ,hunspell)
       ("libidn" ,libidn)
       ("qca" ,qca)
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsvg" ,qtsvg)
       ("qtwebkit" ,qtwebkit)
       ("qtx11extras" ,qtx11extras)
       ("x11" ,libx11)
       ("xext" ,libxext)
       ("xcb" ,libxcb)
       ("zlib" ,zlib)))
    (synopsis "Qt-based XMPP Client")
    (description "Psi is a capable XMPP client aimed at experienced users.
Its design goals are simplicity and stability.")
    (home-page "https://psi-im.org")
    (license license:gpl2+)))

(define-public libgnt
  (package
    (name "libgnt")
    (version "2.14.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/pidgin/libgnt/"
                       version "/libgnt-" version ".tar.xz"))
       (sha256
        (base32 "1n2bxg0ignn53c08cp69pj4sdg53kwlqn23rincyjmpr327fdhsy"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-ncurses-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "meson.build"
               (("'/usr'")
                (string-append "'"
                               (assoc-ref inputs "ncurses")
                               "'")))
             #t))
         (add-before 'configure 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "libgnt-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("ncurses" ,ncurses)))
    (propagated-inputs
     `(("glib" ,glib)
       ("libxml" ,libxml2)
       ("python" ,python-2)))
    (synopsis "GLib Ncurses Toolkit")
    (description "GNT is an ncurses toolkit for creating text-mode graphical
user interfaces in a fast and easy way.  It is based on GLib and ncurses.")
    (home-page "https://keep.imfreedom.org/libgnt/libgnt")
    (license license:gpl2+)))

(define-public libgadu
  (package
    (name "libgadu")
    (version "1.12.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/wojtekka/libgadu.git")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1s16cripy5w9k12534qb012iwc5m9qcjyrywgsziyn3kl3i0aa8h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-shebangs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "protobufgen.sh"
               (("/bin/sh")
                (string-append (assoc-ref inputs "bash")
                               "/bin/sh")))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bash" ,bash)
       ("doxygen" ,doxygen)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("expat" ,expat)
       ("libprotobuf-c" ,protobuf-c)
       ("libxml" ,libxml2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("gnutls" ,gnutls)))
    (synopsis "Library for handling the protocol of Gadu-Gadu")
    (description "LibGadu is library for handling Gadu-Gadu instant messenger
protocol.  The library is written in C and aims to be operating system and
environment independent.")
    (home-page "https://libgadu.net/index.en.html")
    (license license:lgpl2.1+)))

(define-public silc-toolkit
  (package
    (name "silc-toolkit")
    (version "1.1.12")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/silc/silc/toolkit/sources/silc-toolkit-"
                       version ".tar.gz"))
       (sha256
        (base32 "0mnvf9n7qriadg0p7a8qmvcayhnns2g9fhmcymavlm0v8xrky33y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-ipv6"
        "--enable-stack-trace")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'trigger-bootstrap
           (lambda _
             (delete-file "configure")
             (delete-file "Makefile.in")
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (synopsis "SILC ToolKit")
    (description "SILC (Secure Internet Live Conferencing) is a modern and secure
conferencing protocol.  It provides all the common conferencing services like
private messages, instant messages, channels and groups, and video and audio
conferencing.")
    (home-page "https://silc.github.io/info")
    (license
     ;; Dual-licensed
     (list
      license:gpl2+
      license:bsd-2))))

(define-public meanwhile
  (package
    (name "meanwhile")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/obriencj/meanwhile.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1k1gvmx1ikm0y1mdmm495rzkb00pl170jfaf2dy0n5aiiknkk7q3"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("doxygen" ,doxygen)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Library for Lotus Instant Messaging")
    (description "Meanwhile is a library for connecting to a LIM (Lotus Instant
Messaging, formerly Lotus Sametime, formerly VPBuddy) community.  It uses a
protocol based in part off of the IMPP draft(*1), and in part off of traces of
TCP sessions from existing clients.")
    (home-page "https://github.com/obriencj/meanwhile")
    (license license:lgpl3)))

(define-public poezio
  (package
    (name "poezio")
    (version "0.13.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://lab.louiz.org/poezio/poezio.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "041y61pcbdb86s04qwp8s1g6bp84yskc7vdizwpi2hz18y01x5fy"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "setup.py"
               (("'CC', 'cc'")
                "'CC', 'gcc'"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-setuptools" ,python-setuptools)
       ("python-sphinx" ,python-sphinx)))
    (inputs
     `(("python-mpd2" ,python-mpd2)
       ("python-potr" ,python-potr)
       ("python-pyasn1" ,python-pyasn1)
       ("python-pyasn1-modules" ,python-pyasn1-modules)
       ("python-pygments" ,python-pygments)
       ("python-pyinotify" ,python-pyinotify)
       ;("python" ,python)
       ("python-qrcode" ,python-qrcode)
       ("python-slixmpp" ,python-slixmpp)))
    (synopsis "Console Jabber/XMPP Client")
    (description "Poezio is a free console XMPP client (the protocol on which
the Jabber IM network is built).
Its goal is to let you connect very easily (no account creation needed) to the
network and join various chatrooms, immediately.  It tries to look like the
most famous IRC clients (weechat, irssi, etc).  Many commands are identical and
you won't be lost if you already know these clients.  Configuration can be
made in a configuration file or directly from the client.
You'll find the light, fast, geeky and anonymous spirit of IRC while using a
powerful, standard and open protocol.")
    (home-page "https://poez.io/en/")
    (license license:zlib)))

(define-public libotr
  (package
    (name "libotr")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://otr.cypherpunks.ca/libotr-"
                           version ".tar.gz"))
       (sha256
        (base32 "1x8rliydhbibmzwdbyr7pd7n87m2jmxnqkpvaalnf4154hj1hfwb"))
       (patches
        (search-patches "libotr-test-auth-fix.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))                 ; for the test suite
    (inputs
     `(("libgpg-error" ,libgpg-error)))
    (propagated-inputs
     `(("libgcrypt" ,libgcrypt)))    ; libotr headers include gcrypt.h
    (synopsis "Off-the-Record (OTR) Messaging Library and Toolkit")
    (description "OTR allows you to have private conversations over instant
messaging by providing: (1) Encryption: No one else can read your instant
messages.  (2) Authentication: You are assured the correspondent is who you
think it is.  (3) Deniability: The messages you send do not have digital
signatures that are checkable by a third party.  Anyone can forge messages
after a conversation to make them look like they came from you.  However,
during a conversation, your correspondent is assured the messages he sees are
authentic and unmodified.  (4) Perfect forward secrecy: If you lose control of
your private keys, no previous conversation is compromised.")
    (home-page "https://otr.cypherpunks.ca/")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Others
      license:gpl2+))))

(define-public libsignal-protocol-c
  (package
  (name "libsignal-protocol-c")
  (version "2.3.2")
  (source (origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/WhisperSystems/libsignal-protocol-c")
                  (commit (string-append "v" version))))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "1qj2w4csy6j9jg1jy66n1qwysx7hgjywk4n35hlqcnh1kpa14k3p"))))
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
    (version "3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://get.bitlbee.org/src/bitlbee-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0zhhcbcr59sx9h4maf8zamzv2waya7sbsl7w74gbyilvy93dw5cz"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ;; Note: Change to 'check' for versions > 3.6.
                     ("check" ,check-0.12)))
    (inputs `(("glib" ,glib)
              ("libotr" ,libotr)
              ("gnutls" ,gnutls)
              ("python" ,python)
              ("perl" ,perl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-python
           (lambda _ (setenv "PYTHON" (which "python3")) #t))
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
    (home-page "https://www.bitlbee.org/")
    (license (list license:gpl2+ license:bsd-2))))

(define-public bitlbee-purple
  ;; This variant uses libpurple, which provides support for more protocols at
  ;; the expense of a much bigger closure.
  (package/inherit bitlbee
    (name "bitlbee-purple")
    (synopsis "IRC to instant messaging gateway (using Pidgin's libpurple)")
    (inputs `(("purple" ,pidgin)
              ,@(package-inputs bitlbee)))
    (arguments
     (substitute-keyword-arguments (package-arguments bitlbee)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (replace 'configure                    ;add "--purple=1"
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "./configure"
                       (string-append "--prefix="
                                      (assoc-ref outputs "out"))
                       "--otr=1" "--purple=1")))))
       ((#:tests? _ #t)
        ;; XXX: Tests fail to link, and ./configure says that it's "supported
        ;; on a best-effort basis" anyway.
        #f)))))

(define-public bitlbee-discord
  (package
    (name "bitlbee-discord")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sm00th/bitlbee-discord")
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
    (description "Bitlbee-discord is a plugin for Bitlbee which provides
access to servers running the Discord protocol.")
    (home-page "https://github.com/sm00th/bitlbee-discord/")
    (license license:gpl2+)))

(define-public purple-mattermost
  ;; The latest release (1.2) only supports Mattermost's /api/v3.  Choose a
  ;; commit that supports /api/v4.
  (let ((commit "158ce2052af9aaf3d1f6f045f0cfba276e0e91cf")
        (revision "0"))
    (package
      (name "purple-mattermost")
      (version (git-version "1.2" revision commit))
      (home-page "https://github.com/EionRobb/purple-mattermost")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1481zm20pnfq52ncg7hxayjq8cw3a6yh9m4jm1m5s8chsq04015l"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'configure
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        ;; Adjust the makefile to install files in the right
                        ;; place.
                        (let ((out (assoc-ref outputs "out")))
                          (substitute* "Makefile"
                            (("MATTERMOST_DEST = .*")
                             (string-append "MATTERMOST_DEST = " out
                                            "/lib/purple-2\n")) ;XXX: hardcoded
                            (("MATTERMOST_ICONS_DEST = .*")
                             (string-append "MATTERMOST_ICONS_DEST = "
                                            out
                                            "/share/pixmaps/pidgin/protocols\n")))
                          #t))))
         #:make-flags (list "CC=gcc"
                            ,(string-append "PLUGIN_VERSION=" version))
         #:tests? #f))
      (inputs `(("glib" ,glib)
                ("json-glib" ,json-glib)
                ("discount" ,discount)
                ("pidgin" ,pidgin)))
      (native-inputs `(("pkg-config" ,pkg-config)))
      (synopsis "Purple plug-in to access Mattermost instant messaging")
      (description
       "Purple-Mattermost is a plug-in for Purple, the instant messaging library
used by Pidgin and Bitlbee, among others, to access
@uref{https://mattermost.com/, Mattermost} servers.")
      (license license:gpl3+))))

(define-public hexchat
  (package
    (name "hexchat")
    (version "2.14.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.hexchat.net/hexchat/hexchat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "10p829jm1r6kidkgf5lhqhyqc5mxdcq96q3zhadsckasvc9rs6lh"))))
    (build-system meson-build-system)
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin")       ;need glib-genmarshal
                     ("perl" ,perl)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("dbus-glib" ,dbus-glib)
              ("dbus" ,dbus)
              ("enchant" ,enchant)
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
    (synopsis "Graphical IRC client")
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
    (version "26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://arthur.barton.de/pub/ngircd/ngircd-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0m32v0c7mq96rshws4h6d0pi4bm0hynfzx3x01mgrxh9c396zham"))
              (patches (search-patches "ngircd-handle-zombies.patch"))))
    (build-system gnu-build-system)
    ;; Needed for the test suite.
    (native-inputs `(("procps" ,procps)
                     ("expect" ,expect)
                     ("inetutils" ,inetutils)
                     ("openssl" ,openssl)))
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
    (version "2.14.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/pidgin/Pidgin/"
                       version "/pidgin-" version ".tar.gz"))
       (sha256
        (base32 "0vdfnm96m1kh4gm6xn6i7s9c5zjh1p18jg4595k4p5bplvd6fmm8"))
       (patches
        (search-patches "pidgin-add-search-path.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove stale generated file after applying patches.
           (delete-file "configure")
           #t))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("autoconf" ,autoconf) ;; For bootstrap
       ("automake" ,automake) ;; For bootstrap
       ("check" ,check)
       ("dot" ,graphviz)
       ("gconf" ,gconf)
       ("intltool" ,intltool)
       ("libtool" ,libtool) ;; For bootstrap
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("avahi" ,avahi)
       ("cyrus-sasl" ,cyrus-sasl)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ;; ("evolution-data-server" ,evolution-data-server)
       ("farstream" ,farstream)
       ("gnutls" ,gnutls)
       ("gstreamer" ,gstreamer)
       ;; ("gtkspell2" ,gtkspell2)
       ("libgadu" ,libgadu)
       ("libgcrypt" ,libgcrypt)
       ("libgnt" ,libgnt)
       ("libice" ,libice)
       ("libidn" ,libidn)
       ("libltdl" ,libltdl)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxml2" ,libxml2)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxslt" ,libxslt)
       ;; ("libzephyr" ,libzephyr)
       ("meanwhile" ,meanwhile)
       ("mono" ,mono)
       ("ncurses" ,ncurses)
       ("network-manager" ,network-manager)
       ("nspr" ,nspr)
       ("nss" ,nss)
       ("pango" ,pango)
       ("perl" ,perl)
       ("python" ,python-2)
       ("python2-dbus" ,python2-dbus)
       ("silc" ,silc-toolkit)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("tcl" ,tcl)
       ("tk" ,tk)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+-2)))
    (arguments
     `(#:configure-flags
       (list
        (string-append "CFLAGS=-I"
                       (assoc-ref %build-inputs "gst-plugins-base")
                       "/include/gstreamer-1.0")
        "--disable-gtkspell"
        ;; "--enable-gevolution"
        "--enable-cap"
        "--enable-mono"
        "--enable-cyrus-sasl"
        (string-append "--with-ncurses-headers="
                       (assoc-ref %build-inputs "ncurses")
                       "/include")
        (string-append "--with-tclconfig="
                       (assoc-ref %build-inputs "tcl")
                       "/lib")
        (string-append "--with-tkconfig="
                       (assoc-ref %build-inputs "tk")
                       "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'eat-leftovers
           ;; XXX Remove when updating beyond 2.14.3.  Equivalent to
           ;; <https://keep.imfreedom.org/pidgin/pidgin/rev/d4d72fde60c2>.
           (lambda _
             ;; Remove a lingering [broken] oscar reference.
             (substitute* "libpurple/tests/check_libpurple.c"
               ((".*oscar_util_suite.*") "")))))))
    (native-search-paths
     (list
      (search-path-specification
       (variable "PURPLE_PLUGIN_PATH")
       (files
        (list
         (string-append "lib/purple-"
                        (version-major version))
         "lib/pidgin")))))
    (home-page "https://www.pidgin.im/")
    (synopsis "Graphical multi-protocol instant messaging client")
    (description "Pidgin is a modular instant messaging client that supports
many popular chat protocols.")
    (license
     (list
      license:gpl2+   ; Most of the code
      license:lgpl2.1 ; GG protocol plugin (libpurple/protocols/gg/lib)
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
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://otr.cypherpunks.ca/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "1i5s9rrgbyss9rszq6c6y53hwqyw1k86s40cpsfx5ccl9bprxdgl"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+-2)
       ("libgcrypt" ,libgcrypt)
       ("libgpg-error" ,libgpg-error)
       ("libotr" ,libotr)
       ("perl" ,perl)
       ("pidgin" ,pidgin)))
    (home-page "https://otr.cypherpunks.ca/")
    (synopsis "Off-the-Record Messaging plugin for Pidgin")
    (description "Pidgin-OTR is a plugin that adds support for OTR to the Pidgin
instant messaging client.  OTR (Off-the-Record) Messaging allows you to have
private conversations over instant messaging by providing: (1) Encryption: No
one else can read your instant messages.  (2) Authentication: You are assured
the correspondent is who you think it is.  (3) Deniability: The messages you
send do not have digital signatures that are checkable by a third party.  Anyone
can forge messages after a conversation to make them look like they came from
you.  However, during a conversation, your correspondent is assured the messages
he sees are authentic and unmodified.  (4) Perfect forward secrecy: If you lose
control of your private keys, no previous conversation is compromised.")
    (license license:gpl2+)))

(define-public znc
  (package
    (name "znc")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://znc.in/releases/archive/znc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03fyi0j44zcanj1rsdx93hkdskwfvhbywjiwd17f9q1a7yp8l8zz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DWANT_CYRUS=ON"
             "-DWANT_I18N=ON"
             "-DWANT_PERL=ON"
             "-DWANT_PYTHON=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-googletest
           ;; Copy the googletest sources to where the CMake build expects them.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((source (assoc-ref inputs "googletest-source"))
                   (target "third_party/googletest"))
               (mkdir-p target)
               (copy-recursively source target)
               #t))))))
    (native-inputs
     `(("boost" ,boost)
       ("gettext" ,gettext-minimal)
       ("googletest-source" ,(package-source googletest))
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("icu4c" ,icu4c)
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("python" ,python)
       ("zlib" ,zlib)))
    (home-page "https://wiki.znc.in/ZNC")
    (synopsis "IRC network bouncer")
    (description "ZNC is an @dfn{IRC network bouncer} or @dfn{BNC}.  It can
detach the client from the actual IRC server, and also from selected channels.
Multiple clients from different locations can connect to a single ZNC account
simultaneously and therefore appear under the same nickname on IRC.")
    (license license:asl2.0)))

(define-public python-nbxmpp
  (package
    (name "python-nbxmpp")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (pypi-uri "nbxmpp" version))
       (sha256
        (base32 "1482fva70i01w60fk70c0fhqmqgzi1fb4xflllz2v6c8mdqkd1m3"))))
    (build-system python-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")))
    (inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("libsoup" ,libsoup)
       ("python-gssapi" ,python-gssapi)
       ("python-idna" ,python-idna)
       ("python-precis-i18n" ,python-precis-i18n)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Non-blocking XMPP Module")
    (description "Python-nbxmpp is a Python library that provides a way for
Python applications to use the XMPP network.  This library was initially a fork
of xmpppy.")
    (home-page "https://dev.gajim.org/gajim/python-nbxmpp")
    (license license:gpl3+)))

(define-public gajim
  (package
    (name "gajim")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gajim.org/downloads/"
                       (version-major+minor version)
                       "/gajim-" version ".tar.gz"))
       (sha256
        (base32 "070h1n3miq99z6ln77plk3jlisgfqfs2yyn4rhchpf25zd8is1ba"))
       (patches (search-patches "gajim-honour-GAJIM_PLUGIN_PATH.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:imported-modules
       (,@%python-build-system-modules
        (guix build glib-or-gtk-build-system))
       #:modules
       ((guix build python-build-system)
        ((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; ModuleNotFoundError: No module named 'gajim.gui.emoji_data'
             ;; https://dev.gajim.org/gajim/gajim/-/issues/10478
             (delete-file "test/lib/gajim_mocks.py")
             (delete-file "test/unit/test_gui_interface.py")
             #t))
         (replace 'check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "python" "./setup.py" "test")
             #t))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'install 'wrap-env
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (name)
                  (let ((file (string-append out "/bin/" name))
                        (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                        (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                    (wrap-program file
                      `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                      `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))
                '("gajim" "gajim-remote" "gajim-history-manager")))
             #t)))))
    (native-search-paths
     (list
      (search-path-specification
       (variable "GAJIM_PLUGIN_PATH")
       (separator #f)                   ;single entry
       (files
        (list
         "share/gajim/plugins")))
      ;; Gajim needs to use the propagated inputs of its plugins.
      (search-path-specification
       (variable "PYTHONPATH")
       (files
        (list
         (string-append
          "lib/python"
          ;; FIXME: Cannot use this expression as it would
          ;; introduce a circular dependency at the top level.
          ;; (version-major+minor (package-version python))
          "3.8"
          "/site-packages"))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("python-distutils-extra" ,python-distutils-extra)
       ("python-setuptools" ,python-setuptools)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("avahi" ,avahi)
       ("dbus" ,dbus)
       ("farstream" ,farstream)
       ("geoclue" ,geoclue)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gsound",gsound)
       ("gspell" ,gspell)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("gupnp-igd" ,gupnp-igd)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("libxss" ,libxscrnsaver)
       ("network-manager" ,network-manager)
       ("python-css-parser" ,python-css-parser)
       ("python-keyring" ,python-keyring)
       ("python-nbxmpp" ,python-nbxmpp)
       ("python-packaging" ,python-packaging)
       ("python-pillow" ,python-pillow)
       ("python-precis-i18n" ,python-precis-i18n)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pyopenssl" ,python-pyopenssl)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (synopsis "Fully-featured XMPP client")
    (description "Gajim aims to be an easy to use and fully-featured XMPP chat
client.  It is extensible via plugins, supports end-to-end encryption (OMEMO
and OpenPGP) and available in 29 languages.")
    (home-page "https://gajim.org/")
    (license license:gpl3)))

(define-public gajim-omemo
  (package
    (name "gajim-omemo")
    (version "2.7.13")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append
         "https://ftp.gajim.org/plugins_releases/omemo_"
         version ".zip"))
       (sha256
        (base32 "1msr71rvik05wjpa2inpkadddad2rxaqbqcww5qrdrcz75pm8brn"))))
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
     `(("python-axolotl" ,python-axolotl)
       ("python-axolotl-curve25519" ,python-axolotl-curve25519)
       ("python-cryptography" ,python-cryptography)
       ("python-qrcode" ,python-qrcode)))
    (synopsis "Gajim OMEMO plugin")
    (description "Gajim-OMEMO is a plugin that adds support for the OMEMO
Encryption to Gajim.  OMEMO is an XMPP Extension Protocol (XEP) for secure
multi-client end-to-end encryption.")
    (home-page
     "https://dev.gajim.org/gajim/gajim-plugins/-/wikis/OmemoGajimPlugin")
    (license license:gpl3+)))

(define-public gajim-openpgp
  (package
    (name "gajim-openpgp")
    (version "1.3.9")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append
         "https://ftp.gajim.org/plugins_releases/openpgp_"
         version ".zip"))
       (sha256
        (base32 "0fzvvrap1hmj4rbrcjs6cs5c9l9c0795bgw9vxxxk915n6j91m23"))))
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
     `(("python-cryptography" ,python-cryptography)
       ("python-gnupg" ,python-gnupg)
       ("python-gpg" ,python-gpg)))
    (synopsis "Gajim OpenPGP plugin")
    (description "Gajim-OpenPGP is a plugin that adds support for the OpenPGP
Encryption to Gajim.")
    (home-page "https://dev.gajim.org/gajim/gajim-plugins/-/wikis/OpenPGPplugin")
    (license license:gpl3+)))

(define-public dino
  (package
    (name "dino")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/dino/dino/releases/download/v"
                       version "/dino-" version ".tar.gz"))
       (sha256
        (base32 "0iigh7bkil6prf02dqcl6lmd89jxz685h8lqr3ni4x39zkcransn"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:tests? #f
       #:parallel-build? #f             ; not supported
       #:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build cmake-build-system)
                           (guix build glib-or-gtk-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gpgme" ,gpgme)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("libgcrypt" ,libgcrypt)
       ("libgee" ,libgee)
       ("libsignal-protocol-c" ,libsignal-protocol-c)
       ("libsoup" ,libsoup)
       ("qrencode" ,qrencode)
       ("sqlite" ,sqlite)
       ("gpgme" ,gpgme)
       ("gtk+" ,gtk+)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (synopsis "Graphical Jabber/XMPP Client using GTK+/Vala")
    (description "Dino is a chat client for the desktop.  It focuses on providing
a minimal yet reliable Jabber/XMPP experience and having encryption enabled by
default.")
    (home-page "https://dino.im")
    (license license:gpl3+)))

(define-public prosody
  (package
    (name "prosody")
    (version "0.11.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://prosody.im/downloads/source/"
                                  "prosody-" version ".tar.gz"))
              (sha256
               (base32
                "11xz4milv2962qf75vrdwsvd8sy2332nf69202rmvz5989pvvnng"))))
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
                      (url "https://github.com/irungentoo/toxcore")
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
    (version "0.2.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TokTok/c-toxcore")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a6sqpm00d2rn0nviqfz4gh9ck1wzci6rxgmqmcyryl5ca19ffvp"))))
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
   (version "0.18.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/uTox/uTox")
           (commit (string-append "v" version))
           (recursive? #t))) ;; Needed for 'minini' git submodule.
     (file-name (string-append name "-" version "-checkout"))
     (sha256
      (base32
       "01rvlf94d4rkrygnnjak3cg16hrrqyi1rn9nx65y17qk2nbyh68g"))))
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
    "uTox is a lightweight Tox client.  Tox is a distributed and secure
instant messenger with audio and video chat capabilities.")
   (home-page "https://github.com/uTox/uTox")
   (license license:gpl3)))

(define-public qtox
  (package
    (name "qtox")
    (version "1.17.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/qTox/qTox/releases"
                                  "/download/v" version
                                  "/v" version ".tar.gz"))
              (sha256
               (base32
                "11n7si9wdpf80iwkvbspp14dh5jrwm7hxkj8vqhn5pkc48c5bh9j"))
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
         (add-after 'unpack 'disable-network-tests
           (lambda _
             ;; These tests require network access.
             (substitute* "cmake/Testing.cmake"
               (("auto_test\\(core core\\)") "# auto_test(core core)")
               (("auto_test\\(net bsu\\)") "# auto_test(net bsu)"))
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
    (home-page "https://ytalk.ourproject.org")
    (synopsis "Multi-user chat program")
    (description "Ytalk is a replacement for the BSD talk program.  Its main
advantage is the ability to communicate with any arbitrary number of users at
once.  It supports both talk protocols (\"talk\" and \"ntalk\") and can communicate
with several different talk daemons at the same time.")
    (license license:gpl2+)))

(define-public gloox
  (package
    (name "gloox")
    (version "1.0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://camaya.net/download/gloox-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1jgrd07qr9jvbb5hcmhrqz4w4lvwc51m30jls1fgxf1f5az6455f"))))
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
       (uri (string-append "https://perl.psyc.eu/"
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
psycion (a @uref{https://about.psyc.eu,PSYC} chat client), remotor (a control console
for @uref{https://torproject.org,tor} router) and many more.")
    (synopsis "Perl implementation of PSYC protocol")
    (home-page "https://perl.psyc.eu")
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
    (home-page "https://about.psyc.eu/libpsyc")
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
       ("openssl" ,openssl-1.0)
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
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mcabber.com/files/loudmouth/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "03adv5xc84l9brcx0dpyqyffmsclans8yfrpnd357k6x3wfckjri"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:make-flags
       (list
        "CFLAGS=-Wno-error=all")))
    (inputs
     `(("glib" ,glib)
       ("gnutls" ,gnutls)
       ("krb5" ,mit-krb5)
       ("libidn" ,libidn)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("check" ,check)
       ("glib" ,glib "bin")             ; gtester
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
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mcabber.com/files/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32 "0q1i5acyghsmzas88qswvki8kkk2nfpr8zapgnxbcd3lwcxl38f4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-otr"
        "--enable-enchant"
        "--enable-aspell")))
    (inputs
     `(("gpgme" ,gpgme)
       ("libotr" ,libotr)
       ("aspell" ,aspell)
       ("enchant" ,enchant-1.6)
       ("libidn" ,libidn)
       ("glib" ,glib)
       ("ncurses" ,ncurses)
       ("loudmouth" ,loudmouth)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
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
    (version "0.10.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/profanity-im/libmesode")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bxnkhrypgv41qyy1n545kcggmlw1hvxnhwihijhhcf2pxd2s654"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-make
           (lambda _
             (substitute* "Makefile.am"
               (("'\\^xmpp_'") "'.'"))
             #t)))))
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
    (home-page "https://github.com/profanity/libmesode")
    ;; Dual-licensed.
    (license (list license:gpl3+ license:x11))))

(define-public libstrophe
  (package
    (name "libstrophe")
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/strophe/libstrophe")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11d341avsfr0z4lq15cy5dkmff6qpy91wkgzdpfdy31l27pa1g79"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-make
           (lambda _
             (substitute* "Makefile.am"
               (("'\\^xmpp_'") "'.'"))
             #t)))))
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
    ;; Dual-licensed.
    (license (list license:gpl3+ license:x11))))

(define-public profanity
  (package
    (name "profanity")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://profanity-im.github.io/profanity-"
                       version ".tar.gz"))
       (sha256
        (base32
         "137z77514fgj2dk13d12g4jrn6gs5k85nwrk1r1kiv7rj0jy61aa"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-notifications"
        "--enable-python-plugins"
        "--enable-c-plugins"
        "--enable-plugins"
        "--enable-otr"
        "--enable-pgp"
        "--enable-omemo"
        "--enable-icons-and-clipboard")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("autoconf-archive" ,autoconf-archive)
       ("automake" ,automake)
       ("cmocka" ,cmocka)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("expat" ,expat)
       ("glib" ,glib)
       ("gpgme" ,gpgme)
       ("gtk+" ,gtk+-2)
       ("libgcrypt" ,libgcrypt)
       ("libmesode" ,libmesode)
       ("libnotify" ,libnotify)
       ("libotr" ,libotr)
       ("libsignal-protocol-c" ,libsignal-protocol-c)
       ;; ("libxss" ,libxss)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("python" ,python-wrapper)
       ("readline" ,readline)
       ("sqlite" ,sqlite)))
    (synopsis "Console-based XMPP client")
    (description "Profanity is a console based XMPP client written in C
using ncurses and libmesode, inspired by Irssi.")
    (home-page "https://profanity-im.github.io")
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
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JFreegman/toxic")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0p1cmj1kyp506y5xm04mhlznhf5wcylvgsn6b307ms91vjqs3fg2"))
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
             ;; XXX: For compatibility with Python 3.8, adjust python3-config
             ;; invokation to include --embed; see
             ;; <https://github.com/JFreegman/toxic/issues/533>.
             (substitute* "cfg/checks/python.mk"
               (("python3-config --ldflags")
                "python3-config --ldflags --embed"))
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
    (version "0.5.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/quotient-im/libQuotient")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gkwr3yw6k2m0j8cc085b5p2q788rf5nhp1p5hc5d55pc7mci2qs"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)))
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON")
       #:tests? #f))                    ; no tests
    (home-page "https://matrix.org/docs/projects/sdk/libqmatrixclient.html")
    (synopsis "Qt5 client library for the Matrix instant messaging protocol")
    (description "libqmatrixclient is a Qt5 library to write clients for the
Matrix instant messaging protocol.  Quaternion is the reference client
implementation.  Quaternion and libqmatrixclient together form the
QMatrixClient project.")
    (license license:lgpl2.1+)))

(define-public mtxclient
  (package
    (name "mtxclient")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nheko-Reborn/mtxclient")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dg4dq20g0ah62j5s3gpsxqq4ny7lxkxdxa9q6g54hdwkrb9ms7x"))))
    (arguments
     `(#:configure-flags
       (list
        ;; Disable example binaries (not installed)
        "-DBUILD_LIB_EXAMPLES=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-network-tests
           (lambda _
             (substitute* "CMakeLists.txt"
               (("add_test\\((BasicConnectivity|ClientAPI|MediaAPI|Encryption|Pushrules)")
                "# add_test"))
             #t))
         (add-before 'configure 'set-home
           (lambda _
             ;; Tries to create package registry file
             ;; So, set HOME.
             (setenv "HOME" "/tmp")
             #t)))))
    (build-system cmake-build-system)
    (inputs
     `(("boost" ,boost)
       ("json-modern-cxx" ,json-modern-cxx)
       ("libolm" ,libolm)
       ("libsodium" ,libsodium)
       ("openssl" ,openssl)
       ("spdlog" ,spdlog)
       ("zlib" ,zlib)))
    (native-inputs
     `(("googletest" ,googletest)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/Nheko-Reborn/mtxclient")
    (synopsis "Client API library for the Matrix protocol")
    (description "@code{mtxclient} is a C++ library that implements client API
for the Matrix protocol.  It is built on to of @code{Boost.Asio}.")
    (license license:expat)))

(define-public nheko
  (package
    (name "nheko")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nheko-Reborn/nheko")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cbhgaf9klgxdirrxj571fqwspm0byl75c1xc40l727a6qswvp7s"))))
    (arguments
     `(#:tests? #f                      ;no test target
       #:configure-flags
       (list
        "-DCMAKE_BUILD_TYPE=Release"
        "-DCMAKE_CXX_FLAGS=-fpermissive")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-Werror
           (lambda _
             (substitute* "CMakeLists.txt"
               (("-Werror") ""))
             #t))
         (add-after 'unpack 'fix-determinism
           (lambda _
             ;; Make Qt deterministic.
             (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t)))))
    (build-system qt-build-system)
    (inputs
     `(("boost" ,boost)
       ("cmark" ,cmark)
       ("json-modern-cxx" ,json-modern-cxx)
       ("libolm" ,libolm)
       ("lmdb" ,lmdb)
       ("lmdbxx" ,lmdbxx)
       ("mtxclient" ,mtxclient)
       ("openssl" ,openssl)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtmultimedia" ,qtmultimedia)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtsvg" ,qtsvg)
       ("spdlog" ,spdlog)
       ("tweeny" ,tweeny)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools)))
    (home-page "https://github.com/Nheko-Reborn/nheko")
    (synopsis "Desktop client for Matrix using Qt and C++14")
    (description "@code{Nheko} want to provide a native desktop app for the
Matrix protocol that feels more like a mainstream chat app and less like an IRC
client.

There is support for:
@itemize
@item E2E encryption (text messages only: attachments are currently sent unencrypted).
@item User registration.
@item Creating, joining & leaving rooms.
@item Sending & receiving invites.
@item Sending & receiving files and emoji.
@item Typing notifications.
@item Username auto-completion.
@item Message & mention notifications.
@item Redacting messages.
@item Read receipts.
@item Basic communities support.
@item Room switcher (@key{ctrl-K}).
@item Light, Dark & System themes.
@end itemize")
    (license license:gpl3+)))

(define-public quaternion
  (package
    (name "quaternion")
    (version "0.0.9.4f")
    (outputs '("out" "debug"))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/quotient-im/Quaternion")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q9ddz4rs02a0w3lwrsjnh59khv38cq9f0kv09vnwvazvayn87ck"))))
    (build-system qt-build-system)
    (inputs
     `(("libqmatrixclient" ,libqmatrixclient)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtsvg" ,qtsvg)
       ("qttools" ,qttools)
       ("xdg-utils" ,xdg-utils)))
    (arguments
     `(#:tests? #f))                    ; no tests
    (home-page "https://matrix.org/docs/projects/client/quaternion.html")
    (synopsis "Graphical client for the Matrix instant messaging protocol")
    (description "Quaternion is a Qt5 desktop client for the Matrix instant
messaging protocol.  It uses libqmatrixclient and is its reference client
implementation.  Quaternion and libqmatrixclient together form the
QMatrixClient project.")
    (license (list license:gpl3+        ; all source code
                   license:lgpl3+))))   ; icons/breeze

(define-public hangups
  (package
    (name "hangups")
    (version "0.4.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hangups" version))
       (sha256
        (base32 "015g635vnrxk5lf9n80rdcmh6chv8kmla1k2j7m1iijijs519ngn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'relax-dependencies
           ;; Relax overly strict package version specifications.
           (lambda _
             (substitute* "setup.py"
               (("==") ">=")
               ((",<.*'") "'"))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "hangups"))
             #t)))))
    (propagated-inputs
     `(("python-aiohttp" ,python-aiohttp)
       ("python-appdirs" ,python-appdirs)
       ("python-async-timeout" ,python-async-timeout)
       ("python-configargparse" ,python-configargparse)
       ("python-mechanicalsoup" ,python-mechanicalsoup)
       ("python-protobuf" ,python-protobuf-3.6)
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
    (version "1.4.2")
    (home-page "https://github.com/majn/telegram-purple")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (sha256
               (base32
                "0imbzhhq9qbj6gvkckrnjhls2vvmmy8db7l6gsd7lng2pbfcn522"))
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
       ("gettext" ,gettext-minimal)
       ("which" ,which)))
    (inputs
     `(("pidgin" ,pidgin)
       ("libgcrypt" ,libgcrypt)
       ("libwebp" ,libwebp)
       ("glib" ,glib)
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

(define-public tdlib
  (package
    (name "tdlib")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tdlib/td")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0dfir57ljcn98mkg061c5642qb93wh2lm1n4nngpl3na9vvfk75i"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #t
       #:configure-flags
       (list "-DCMAKE_BUILD_TYPE=Release"
             "-DTD_ENABLE_LTO=OFF") ; FIXME: Get LTO to work.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-failing-tests
           (lambda _
             (substitute* "test/CMakeLists.txt"
               ;; The test cases are compiled into a distinct binary
               ;; which uses mtproto.cpp to attempt to connect to
               ;; a remote server. Removing this file from the sources
               ;; list disables those specific test cases.
               (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/mtproto.cpp") ""))
             #t)))))
    (native-inputs
     `(("gperf" ,gperf)
       ("openssl" ,openssl)
       ("zlib" ,zlib)
       ("php" ,php)
       ("doxygen" ,doxygen)))
    (synopsis "Cross-platform library for building Telegram clients")
    (description "Tdlib is a cross-platform library for creating custom
Telegram clients following the official Telegram API.  It can be easily used
from almost any programming language with a C-FFI and features first-class
support for high performance Telegram Bot creation.")
    (home-page "https://core.telegram.org/tdlib")
    (license license:boost1.0)))

(define-public purple-mm-sms
  (package
    (name "purple-mm-sms")
    (version "0.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://source.puri.sm/Librem5/purple-mm-sms.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1daf7zl8bhhm1szkgxflpqql69f2w9i9nlgf1n4p1nynxifz1bim"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         ;; Fix hardcoded paths
         (list (string-append "PREFIX=" out)
               (string-append "PLUGIN_DIR_PURPLE=" out "/lib/purple-2")
               (string-append "DATA_ROOT_DIR_PURPLE=" out "/share")))
       #:tests? #f      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("modem-manager" ,modem-manager)
       ("pidgin" ,pidgin)))
    (synopsis "Libpurple plugin for SMS via ModemManager")
    (description "Plugin for libpurple to allow sending SMS using ModemManager.")
    (home-page "https://source.puri.sm/Librem5/purple-mm-sms")
    (license license:gpl2+)))

(define-public chatty
 (package
   (name "chatty")
   (version "0.1.17")
   (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://source.puri.sm/Librem5/chatty.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ba1rw8a3vif9k3570hxjfm25vqys3vk3f6g8z5irklwq4bi6lmn"))))
   (build-system meson-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'skip-updating-desktop-database
          (lambda _
            (substitute* "meson.build"
              (("meson.add_install_script.*") ""))
            #t)))))
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("feedbackd" ,feedbackd)
      ("folks" ,folks)
      ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
      ("libgcrypt" ,libgcrypt)
      ("libgee" ,libgee)
      ("libhandy" ,libhandy-0.0)
      ("pidgin" ,pidgin)
      ("purple-mm-sms" ,purple-mm-sms)
      ("sqlite" ,sqlite)))
   (propagated-inputs
    `(("adwaita-icon-theme" ,adwaita-icon-theme)
      ("evolution-data-server" ,evolution-data-server)))
   (synopsis "Mobile client for XMPP and SMS messaging")
   (description "Chatty is a chat program for XMPP and SMS.  It works on mobile
as well as on desktop platforms.  It's based on libpurple and ModemManager.")
   (home-page "https://source.puri.sm/Librem5/chatty")
   (license license:gpl3+)))

(define-public mosquitto
  (package
    (name "mosquitto")
    (version "1.6.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mosquitto.org/files/source/mosquitto-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1yq7y329baa1ly488rw125c3mvsnsa7kjkik602xv1xpkz8p73al"))))
    (build-system cmake-build-system)
    (inputs
     `(("openssl" ,openssl)))
    (synopsis "Message broker")
    (description "This package provides Eclipse Mosquitto, a message broker
that implements the MQTT protocol versions 5.0, 3.1.1 and 3.1.  Mosquitto
is lightweight and is suitable for use on all devices from low power single
board computers to full servers.

The MQTT protocol provides a lightweight method of carrying out messaging
using a publish/subscribe model. This makes it suitable for Internet of
Things messaging such as with low power sensors or mobile devices such
as phones, embedded computers or microcontrollers.")
    (home-page "https://mosquitto.org/")
    ;; Dual licensed.
    (license (list license:epl1.0 license:edl1.0))))

(define-public movim-desktop
  (let ((commit "83d583b83629dbd2ec448da9a1ffd81f6c1fb295")
        (revision "3"))
    (package
      (name "movim-desktop")
      (version
       (git-version "0.14.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/movim/movim_desktop")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1lsa3k3wx1d2lk0qs0k5jc5bmapnmpzwynprjf2wihh8c8y3iwlz"))))
      (build-system qt-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* `("CMakeLists.txt" "movim.desktop")
                 (("/usr")
                  (assoc-ref outputs "out"))
                 (("\"build")
                  "\"../build"))
               #t)))))
      (inputs
       `(("qtbase" ,qtbase)
         ("qtdeclarative" ,qtdeclarative)
         ("qtwebchannel" ,qtwebchannel)))
      (propagated-inputs
       `(("qtwebengine" ,qtwebengine)))
      (home-page "https://movim.eu/")
      (synopsis "Desktop Application for Movim")
      (description
       "Movim-Desktop is a desktop application, relying on Qt, for the Movim
social and chat platform.")
      (license license:gpl3+))))

(define-public psi-plus
  (package
    (name "psi-plus")
    (version "1.5.1482")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/psi-plus/psi-plus-snapshots")
         (commit version)))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        `(begin
           (delete-file-recursively "3rdparty")))
       (sha256
        (base32 "0lcx616hchwf492m1dm8ddb4qd2pmgf703ajnnb0y9ky99kgg8q2"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:imported-modules
       (,@%qt-build-system-modules
        (guix build glib-or-gtk-build-system))
       #:modules
       ((guix build qt-build-system)
        ((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build utils))
       #:configure-flags
       (list
        "-DBUILD_PSIMEDIA=ON"           ; For A/V support
        "-DENABLE_PLUGINS=ON"
        "-DUSE_HUNSPELL=OFF"            ; Use Enchant instead
        "-DUSE_ENCHANT=ON"
        "-DUSE_CCACHE=OFF")             ; Not required
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "CMakeLists.txt"
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/http-parser/http_parser.h")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qhttp/qhttp.pro")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qite/qite.pro")
                "")
               (("add_subdirectory\\( 3rdparty \\)")
                ""))
             (substitute* "src/CMakeLists.txt"
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qite/libqite")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/http-parser")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qhttp/src/private")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qhttp/src")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty")
                "")
               (("add_dependencies\\(\\$\\{PROJECT_NAME\\} qhttp\\)")
                "target_link_libraries(${PROJECT_NAME} qhttp)"))
             (substitute* "src/src.cmake"
               (("include\\(\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qite/libqite/libqite.cmake\\)")
                "list(APPEND EXTRA_LIBS qite)"))
             (substitute* '("src/filesharingmanager.h" "src/widgets/psirichtext.cpp"
                            "src/filesharingmanager.cpp" "src/widgets/psitextview.cpp"
                            "src/chatview_te.cpp" "src/msgmle.cpp")
               (("qite.h")
                "qite/qite.h")
               (("qiteaudio.h")
                "qite/qiteaudio.h")
               (("qiteaudiorecorder.h")
                "qite/qiteaudiorecorder.h"))
             #t))
         (add-after 'install 'wrap-env
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (name)
                  (let ((file (string-append out "/bin/" name))
                        (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                        (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                    (wrap-program file
                      `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                      `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))
                '("psi-plus")))
             #t))
         (add-after 'wrap-env 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("ruby" ,ruby)))
    (inputs
     `(("blake2" ,libb2)
       ("dbus" ,dbus)
       ("enchant" ,enchant)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("http-parser" ,http-parser)
       ("libgcrypt" ,libgcrypt)
       ("libgpg-error" ,libgpg-error)
       ("libidn" ,libidn)
       ("libotr" ,libotr)
       ("libsignal-protocol-c" ,libsignal-protocol-c)
       ("libtidy" ,tidy-html)
       ("openssl" ,openssl)
       ("qca" ,qca)
       ("qhttp" ,qhttp)
       ("qite" ,qite)
       ("qtbase" ,qtbase)
       ("qtkeychain" ,qtkeychain)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("usrsctp" ,usrsctp)
       ("x11" ,libx11)
       ("xext" ,libxext)
       ("xcb" ,libxcb)
       ("xss" ,libxscrnsaver)
       ("zlib" ,zlib)))
    (home-page "https://psi-plus.com/")
    (synopsis "Qt-based XMPP Client")
    (description
     "Psi+ is a spin-off of Psi XMPP client.  It is a powerful XMPP client
designed for experienced users.")
    (license license:gpl2+)))

(define-public python-zulip
  (package
    (name "python-zulip")
    (version "0.7.1")
    (source
     (origin
       ;; There is no source on Pypi.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zulip/python-zulip-api")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0da1ki1v252avy27j6d7snnc0gyq0xa9fypm3qdmxhw2w79d6q36"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'cd-to-zulip-dir
           (lambda _
             (chdir "zulip")
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (let ((test-zulip "../tools/test-zulip"))
               (when tests?
                 (add-installed-pythonpath inputs outputs)
                 (setenv "PYTHONPATH" (string-append ".:" (getenv "PYTHONPATH")))
                 (patch-shebang test-zulip)
                 (invoke test-zulip))
               #t))))))
    (propagated-inputs
     `(("python-matrix-client" ,python-matrix-client)
       ("python-pyopenssl" ,python-pyopenssl)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-distro" ,python-distro)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/zulip/python-zulip-api")
    (synopsis "Zulip's API Python bindings")
    (description
     "This package provides Python bindings to Zulip's API.")
    (license license:asl2.0)))

(define-public zulip-term
  (package
    (name "zulip-term")
    (version "0.5.2")
    (source
     (origin
       ;; Pypi package doesn't ship tests.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zulip/zulip-terminal")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1xhhy3v4wck74a83avil0rnmsi2grrh03cww19n5mv80p2q1cjmf"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "setup.py"
             (("\\=\\=1\\.7") ">=1.7")  ; pytest-mock
             (("\\=\\=2\\.5") ">=2.5")  ; pytest-cov
             (("4\\.5\\.2") "4.4.2"))   ; lxml
           #t))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Delete failing tests.
               (delete-file "tests/cli/test_run.py")
               (invoke "pytest"))
             #t)))))
    (inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-lxml" ,python-lxml)
       ("python-mypy-extensions" ,python-mypy-extensions)
       ("python-urwid" ,python-urwid)
       ("python-urwid-readline" ,python-urwid-readline)
       ("python-zulip" ,python-zulip)))
    (native-inputs
     `(("python-distro" ,python-distro)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)))
    (home-page "https://github.com/zulip/zulip-terminal")
    (synopsis "Zulip's official terminal client")
    (description "This package contains Zulip's official terminal client.")
    (license license:asl2.0)))

;;; messaging.scm ends here
