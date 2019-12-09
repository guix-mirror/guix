;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages kde-internet)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml))

(define-public choqok
  (package
    (name "choqok")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/choqok/"
                           (version-major+minor version)
                           "/src/choqok-" version ".tar.xz"))
       (sha256
        (base32 "03ri4y1wzyqlixnhczsls5gmy7jzzm67bb5gz8bav51ngc32fxca"))
       (patches (search-patches "choqok-Fix-building-under-Qt-5.13.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("attica" ,attica)
       ("kcmutils" ,kcmutils)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdewebkit" ,kdewebkit)
       ("kemoticons" ,kemoticons)
       ("kglobalaccel" ,kglobalaccel)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kparts" ,kparts)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ;; TODO: telepathy
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("qca" ,qca)
       ("qoauth" ,qoauth)
       ("qtbase" ,qtbase)
       ("qtwebkit" ,qtwebkit)
       ("sonnet" ,sonnet)))
    (home-page "https://kde.org/applications/internet/org.kde.choqok")
    (synopsis "Micro-Blogging Client")
    (description "Choqok is a fast, efficient and simple to use micro-blogging
client for KDE.  It currently supports the twitter.com and identi.ca
microblogging services.

Other notable features include:
@itemize
@item Support for user + friends time-lines.
@item Support for @Reply time-lines.
@item Support for sending and receiving direct messages.
@item Twitpic.com integration.
@item The ability to use multiple accounts simultaneously.
@item Support for search APIs for all services.
@item KWallet integration.
@item Support for automatic shortening urls with more than 30 characters.
@item Support for configuring status lists appearance.
@end itemize")
    (license license:gpl3+)))

(define-public kget
  (package
    (name "kget")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kget-" version ".tar.xz"))
       (sha256
        (base32 "004qqq93iqidh2m9q2p2cwlbc2kfrz0g8a2mgd712c9p66l7s42s"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("gmp" ,gmp)
       ("gpgme" ,gpgme)
       ("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdelibs4support" ,kdelibs4support) ;; KLocale
       ("kdoctools" ,kdoctools)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kparts" ,kparts)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libgcrypt" ,libgcrypt)
       ("libktorrent" ,libktorrent)
       ;; TODO: libmms
       ;; TODO: LibKWorkspace - plasma-workspace?
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("qca" ,qca)
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)
       ))
    (home-page "http://www.kde.org/")
    (synopsis "Versatile and user-friendly download manager")
    (description "KGet is an advanced download manager with support for
Metalink and Bittorrent.  Downloads are added to the list, where they can be
paused, queued, or scheduled for later. KGet supports download via FTP anf
HTTP(S) as well as pausing downloads.

This package is part of the KDE networking module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public konversation
  (package
    (name "konversation")
    (version "1.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/konversation/" version
                           "/src/konversation-" version ".tar.xz"))
       (sha256
        (base32 "0h098yhlp36ls6pdvs2r93ig8dv4fys62m0h6wxccprb0qrpbgv0"))
       (patches (search-patches "konversation-Fix-build-with-Qt-5.11.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("karchive" ,karchive)
       ("kbookmarks" ,kbookmarks)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kemoticons" ,kemoticons)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidletime" ,kidletime)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kparts" ,kparts)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qca" ,qca)
       ("solid" ,solid)
       ("sonnet" ,sonnet)))
    (home-page "https://kde.org/applications/internet/org.kde.konversations")
    (synopsis "Graphical Internet Relay Chat (IRC) client for KDE")
    (description "Konversation is a graphical Internet Relay Chat client (IRC)
with KDE support.

Features are:
@itemize
@item Standard IRC features
@item SSL server support
@item Bookmarking support
@item Easy to use graphical user interface
@item Multiple servers and channels in one single window
@item DCC file transfer with resume support
@item Multiple identities for different servers
@item Text decorations and colors
@item Pattern-based message highlighting and OnScreen Display notifications
@item Automatic UTF-8 detection
@item Per channel encoding support
@item Theme support for nick icons
@item Highly configurable
@item Multi-language scripting support (with DCOP)
@item Customizable command aliases
@item NickServ-aware log-on (for registered nicknames)
@item Smart logging
@item Traditional or enhanced-shell-style nick completion
@end itemize")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kopete
  (package
    (name "kopete")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kopete-" version ".tar.xz"))
       (sha256
        (base32 "088jya4v04l7r38pph1hxkr6ln4023s3ji3y8ipzdkalcx8hgr6l"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("expat" ,expat)
       ("glib" ,glib)
       ("gpgme" ,gpgme)
       ("jsoncpp" ,jsoncpp)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdelibs4support" ,kdelibs4support)
       ("kdnssd" ,kdnssd)
       ("kemoticons" ,kemoticons)
       ("khtml" ,khtml)
       ("ki18n" ,ki18n)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kjs" ,kjs)
       ;; TODO? kleopatra (additionally to libkleo)
       ("knotifyconfig" ,knotifyconfig)
       ("kparts" ,kparts)
       ("kpimtextedit" ,kpimtextedit)
       ("ktexteditor" ,ktexteditor)
       ("kwallet" ,kwallet)
       ;; TODO: Libgadu
       ("libidn" ,libidn)
       ("libkleo" ,libkleo)
       ;; TODO: LibMeanwhile
       ("libotr" ,libotr)
       ("libsrtp" ,libsrtp)
       ("libxml2" ,libxml2)
       ("libxstl" ,libxslt)
       ;; TODO: Mediastreamer
       ("openssl", openssl)
       ("ortp" ,ortp)
       ("phonon" ,phonon)
       ("qca" ,qca)
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)
       ("speex" ,speex)
       ("v4l-utils" ,v4l-utils)
       ;; TODO: Xmms
       ("zlib" ,zlib)))
    ;; TODO: enable video support
    (home-page "https://kde.org/applications/internet/org.kde.kopete")
    (synopsis "Instant messaging and chat application")
    (description "Kopete is an instant messenger supporting Jabber/XMPP ,AIM,
ICQ, Gadu-Gadu, Novell GroupWise Messenger, and more.  It is designed to be a
flexible and extensible multi-protocol system suitable for personal and
enterprise use.

The goal of Kopete is to provide users with a single easy-to-use way to access
all of their instant messaging systems.  The interface puts people first, and
is integrated with the system address book to let you access your contacts
from other KDE applications.

This package is part of the KDE networking module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public krdc
  (package
    (name "krdc")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/krdc-" version ".tar.xz"))
       (sha256
        (base32 "1p6g994whzjz9aarzrblh70xzs3jvygd1898qxgfymndlfxaxjyl"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kbookmarks" ,kbookmarks)
       ("freerdp" ,freerdp)
       ("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kdnssd" ,kdnssd)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("knotifyconfig" ,knotifyconfig)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libssh2" ,libssh)
       ;; TODO: libvnc{server,client} - is not tigervnc-{server,client}
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/internet/org.kde.krdc")
    (synopsis "Remote desktop client")
    (description "KRDC is a client application that allows you to view or even
control the desktop session on another machine that is running a compatible
server.  VNC and RDP are supported.

This package is part of the KDE networking module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public libktorrent
  (package
    (name "libktorrent")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       ;; TODO: Base on version of ktorrent
       (uri (string-append "mirror://kde//stable/ktorrent/5.1.2"
                           "/libktorrent-" version ".tar.xz"))
       (sha256
        (base32 "0051zh8bb4p9wmcfn5ql987brhsaiw9880xdck7b5dm1a05mri2w"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("boost" ,boost)
       ("gmp" ,gmp)
       ("karchive" ,karchive)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("libgcrypt" ,libgcrypt)
       ("qca" ,qca)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
    (home-page "https://cgit.kde.org/libktorrent.git")
    (synopsis "BitTorrent protocol library for C++ / Qt 5 / KDE Frameworks")
    (description "The KTorrent library supports connectivity to HTTP and UDP
trackers, mainline DHT and the new generation Micro Transport
Protocol (uTP).  In addition, it provides many powerful BitTorrent network
features including but not limited to torrent downloading and seeding, torrent
creation and downloaded data verification, magnet links, advanced peer
management, IP blocking lists.")
    (license license:gpl2+)))
