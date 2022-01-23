;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2018, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Josselin Poiret <josselin.poiret@protonmail.ch>
;;; Copyright © 2022 Brice Waegeneire <brice@waegenei.re>
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

(define-module (gnu packages bittorrent)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public transmission
  (package
    (name "transmission")
    (version "3.00")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/transmission/transmission"
                                  "/releases/download/" version "/transmission-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1wjmn96zrvmk8j1yz2ysmqd7a2x6ilvnwwapcvfzgxs2wwpnai4i"))
              (patches (search-patches "transmission-honor-localedir.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out"                      ; library and command-line interface
               "gui"))                    ; graphical user interface
    (arguments
     '(#:configure-flags
       (list (string-append "--localedir="
                            (assoc-ref %outputs "gui")
                            "/share/locale"))
       ;; Some tests segfault when using libevent 2.12 without internet
       ;; connection. This has been reported mainstream but not fixed yet:
       ;; https://github.com/transmission/transmission/issues/1437.
       #:tests? #f
       #:glib-or-gtk-wrap-excluded-outputs '("out")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-gui
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move the GUI to its own output, so that "out" doesn't
             ;; depend on GTK+.
             (let ((out (assoc-ref outputs "out"))
                   (gui (assoc-ref outputs "gui")))
               (mkdir-p (string-append gui "/bin"))
               (rename-file (string-append out "/bin/transmission-gtk")
                            (string-append gui "/bin/transmission-gtk"))

               (for-each
                (lambda (dir)
                  (rename-file (string-append out "/share/" dir)
                               (string-append gui "/share/" dir)))
                '("appdata" "applications" "icons" "pixmaps"))

               (mkdir-p (string-append gui "/share/man/man1"))
               (rename-file
                (string-append out "/share/man/man1/transmission-gtk.1")
                (string-append gui "/share/man/man1/transmission-gtk.1"))
             #t))))))
    (inputs
     (list libevent curl openssl zlib gtk+))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://transmissionbt.com/")
    (synopsis "Fast and easy BitTorrent client")
    (description
     "Transmission is a BitTorrent client that comes with graphical,
textual, and Web user interfaces.  Transmission also has a daemon for
unattended operations.  It supports local peer discovery, full encryption,
DHT, µTP, PEX and Magnet Links.")

    ;; COPYING reads:
    ;;
    ;;     Transmission can be redistributed and/or modified under the terms of
    ;; the GNU GPLv2 (http://www.gnu.org/licenses/license-list.html#GPLv2),
    ;; the GNU GPLv3 (http://www.gnu.org/licenses/license-list.html#GNUGPLv3),
    ;; or any future license endorsed by Mnemosyne LLC.
    ;;
    ;; A few files files carry an MIT/X11 license header.
    (license (list l:gpl2 l:gpl3))))

(define-public transmission-remote-gtk
  (package
    (name "transmission-remote-gtk")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/transmission-remote-gtk/"
                           "transmission-remote-gtk/releases/download/"
                           version "/transmission-remote-gtk-" version
                           ".tar.gz"))
       (sha256
        (base32 "0qz9wi70qc6vgnaymivc3xz6y86c9hglk6wjv3snnqxpxmp9saay"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gnu-gettext pkg-config))
    (inputs
     (list appstream-glib curl gtk+ json-glib))
    (synopsis "Gtk frontend to the Transmission daemon")
    (description "transmission-remote-gtk is a GTK client for remote management
of the Transmission BitTorrent client, using its HTTP RPC protocol.")
    (home-page "https://github.com/transmission-remote-gtk/transmission-remote-gtk")
    (license l:gpl2+)))

(define-public libtorrent
  (package
    (name "libtorrent")
    (version "0.13.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://rtorrent.net/downloads/libtorrent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "10z9i1rc41cmmi7nx8k7k1agsx6afv09g9cl7g9zr35fyhl5l4gd"))))
    (build-system gnu-build-system)
    (inputs (list openssl zlib))
    (native-inputs (list pkg-config cppunit))
    (synopsis "BitTorrent library of rtorrent")
    (description
     "LibTorrent is a BitTorrent library used by and developed in parallel
with the BitTorrent client rtorrent.  It is written in C++ with emphasis on
speed and efficiency.")
    (home-page "https://github.com/rakshasa/libtorrent")
    (license l:gpl2+)))

(define-public rtorrent
  (package
    (name "rtorrent")
    (version "0.9.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://rtorrent.net/downloads/rtorrent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1bs2fnf4q7mlhkhzp3i1v052v9xn8qa7g845pk9ia8hlpw207pwy"))))
    (build-system gnu-build-system)
    (inputs (list libtorrent
                  ncurses
                  curl
                  cyrus-sasl
                  openssl
                  zlib))
    (native-inputs (list pkg-config cppunit))
    (synopsis "BitTorrent client with ncurses interface")
    (description
     "rTorrent is a BitTorrent client with an ncurses interface.  It supports
full encryption, DHT, PEX, and Magnet Links.  It can also be controlled via
XML-RPC over SCGI.")
    (home-page "https://github.com/rakshasa/rtorrent")
    (license l:gpl2+)))

(define-public tremc
  (package
    (name "tremc")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tremc/tremc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fqspp2ckafplahgba54xmx0sjidx1pdzyjaqjhz0ivh98dkx2n5"))
       (patches (search-patches "tremc-fix-decodestring.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; The software is just a Python script that must be copied into place.
         (delete 'configure)
         (delete 'build))))
    (inputs
     (list python))
    (synopsis "Console client for the Transmission BitTorrent daemon")
    (description "Tremc is a console client, with a curses interface, for the
Transmission BitTorrent daemon.")
    (home-page "https://github.com/tremc/tremc")
    (license l:gpl3+)))

(define-public transmission-remote-cli
  (package
    (name "transmission-remote-cli")
    (version "1.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fagga/transmission-remote-cli")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09w9f8vrm61lapin8fmq4rgahr95y3c6wss10g0fgd0kl16f895v"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; only supports Python 2
       #:tests? #f ; no test suite
       #:phases (modify-phases %standard-phases
                  ;; The software is just a Python script that must be
                  ;; copied into place.
                  (delete 'build)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (man (string-append out "/share/man/man1"))
                             ;; FIXME install zsh completions
                             (completions (string-append out "/etc/bash_completion.d")))
                        (install-file "transmission-remote-cli" bin)
                        (install-file "transmission-remote-cli.1" man)
                        (install-file
                          (string-append
                            "completion/bash/"
                            "transmission-remote-cli-bash-completion.sh")
                          completions)))))))
    (synopsis "Console client for the Transmission BitTorrent daemon")
    (description "Transmission-remote-cli is a console client, with a curses
interface, for the Transmission BitTorrent daemon.  This package is no longer
maintained upstream.")
    (home-page "https://github.com/fagga/transmission-remote-cli")
    (license l:gpl3+)
    (properties `((superseded . ,tremc)))))

(define-public aria2
  (package
    (name "aria2")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aria2/aria2/releases/"
                                  "download/release-" version
                                  "/aria2-" version ".tar.xz"))
              (sha256
               (base32
                "1987x4ywnnrhhfs9hi2h820c200d7nas9nd35414yh0jiihfglaq"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list "--enable-libaria2"
               (string-append "--with-bashcompletiondir="
                              #$output "/etc/bash_completion.d/"))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'delete-socket-tests
             (lambda _
               (substitute* "test/LpdMessageDispatcherTest.cc"
                 (("CPPUNIT_TEST_SUITE_REGISTRATION\\(LpdMessageDispatcherTest\\);" text)
                  (string-append "// " text)))
               (substitute* "test/LpdMessageReceiverTest.cc"
                 (("CPPUNIT_TEST_SUITE_REGISTRATION\\(LpdMessageReceiverTest\\);" text)
                  (string-append "// " text))))))))
    (native-inputs
     (list cppunit ; for the tests
           pkg-config))
    (inputs
     (list c-ares
           gnutls
           gmp
           libssh2
           libxml2
           nettle
           sqlite
           zlib))
    (home-page "https://aria2.github.io/")
    (synopsis "Utility for parallel downloading files")
    (description
      "Aria2 is a lightweight, multi-protocol & multi-source command-line
download utility.  It supports HTTP/HTTPS, FTP, SFTP, BitTorrent and Metalink.
Aria2 can be manipulated via built-in JSON-RPC and XML-RPC interfaces.")
    (properties
     '((release-monitoring-url . "https://github.com/aria2/aria2/releases")))
    (license l:gpl2+)))

(define-public uget
  (package
    (name "uget")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/urlget/"
                           "uget%20%28stable%29/" version "/uget-"
                           version ".tar.gz"))
       (sha256
        (base32 "0dlrjhnm1pg2vwmp7nl2xv1aia5hyirb3021rl46x859k63zap24"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")))
    (inputs
     (list curl
           gtk+
           glib
           gnutls
           gstreamer
           libgcrypt
           libnotify
           openssl))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://ugetdm.com/")
    (synopsis "Universal download manager with GTK+ interface")
    (description
     "uGet is portable download manager with GTK+ interface supporting
HTTP, HTTPS, BitTorrent and Metalink, supporting multi-connection
downloads, download scheduling, download rate limiting.")
    (license l:lgpl2.1+)))

(define-public mktorrent
  (package
    (name "mktorrent")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Rudde/mktorrent")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17pdc5mandl739f8q26n5is8ga56s83aqcrwhlnnplbxwx2inidr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))          ; no configure script
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "NO_HASH_CHECK=1"
                          "USE_LARGE_FILES=1"
                          "USE_LONG_OPTIONS=1"
                          "USE_PTHREADS=1")
       #:tests? #f))                            ; no tests
    (home-page "https://github.com/Rudde/mktorrent")
    (synopsis "Utility to create BitTorrent metainfo files")
    (description
     "mktorrent is a simple command-line utility to create BitTorrent
@dfn{metainfo} files, often known simply as @dfn{torrents}, from both single
files and whole directories.  It can add multiple trackers and web seed URLs,
and set the @code{private} flag to disallow advertisement through the
distributed hash table (@dfn{DHT}) and Peer Exchange.  Hashing is multi-threaded
and will take advantage of multiple processor cores where possible.")
    (license (list l:public-domain      ; sha1.*, used to build without OpenSSL
                   l:gpl2+))))          ; with permission to link with OpenSSL

(define-public libtorrent-rasterbar
  (package
    (name "libtorrent-rasterbar")
    (version "1.2.15")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/arvidn/libtorrent/"
                       "releases/download/v" version "/"
                       "libtorrent-rasterbar-" version ".tar.gz"))
       (sha256
        (base32 "0jr1c876mvwbbbnav8ldcdm1l6z3g404jc5wp8z902jcd0w8dbf8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-Dpython-bindings=ON"
                           "-Dbuild_tests=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'extend-test-timeout
           (lambda _
             (substitute* "test/test_remove_torrent.cpp"
               ;; Extend the test timeout from 3 seconds to 10.
               (("i > 30") "i > 100"))))
         (replace 'check
           (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
             (let ((disabled-tests
                    ;; test_upnp requires a non-localhost IPv4 interface.
                    '("test_upnp")))
               (when tests?
                 ;; test_ssl relies on bundled TLS certificates with a fixed
                 ;; expiry date.  To ensure succesful builds in the future,
                 ;; fake the time to be roughly that of the release.
                 (setenv "FAKETIME_ONLY_CMDS" "test_ssl")
                 (invoke "faketime" "2021-12-12"
                         "ctest"
                         "--exclude-regex" (string-join disabled-tests "|")
                         "-j" (if parallel-tests?
                                  (number->string (parallel-job-count))
                                  "1")
                         "--rerun-failed"
                         "--output-on-failure"))))))))
    (inputs (list boost openssl))
    (native-inputs
     (list libfaketime
           python-wrapper
           pkg-config))
    (home-page "https://www.libtorrent.org/")
    (synopsis "Feature-complete BitTorrent implementation")
    (description
     "libtorrent-rasterbar is a feature-complete C++ BitTorrent implementation
focusing on efficiency and scalability.  It runs on embedded devices as well as
desktops.")
    (license l:bsd-2)))

(define-public qbittorrent
  (package
    (name "qbittorrent")
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/qbittorrent/qBittorrent")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aqrcwxi3s2alila3fa7fjs4hifkq7055wa4xvz17hajchs3l567"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir="
                            (assoc-ref %build-inputs "boost")
                            "/lib")
             "--enable-debug"
             "QMAKE_LRELEASE=lrelease")
       #:modules ((guix build gnu-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build qt-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-qt
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-qt-program "qbittorrent" #:output out #:inputs inputs))
             #t)))))
    (native-inputs
     (list pkg-config qttools))
    (inputs
     `(("boost" ,boost)
       ("libtorrent-rasterbar" ,libtorrent-rasterbar)
       ("openssl" ,openssl)
       ("python" ,python-wrapper)
       ("qtbase" ,qtbase-5)
       ("qtsvg" ,qtsvg)
       ("zlib" ,zlib)))
    (home-page "https://www.qbittorrent.org/")
    (synopsis "Graphical BitTorrent client")
    (description
     "qBittorrent is a BitTorrent client programmed in C++/Qt that uses
libtorrent (sometimes called libtorrent-rasterbar) by Arvid Norberg.

It aims to be a good alternative to all other BitTorrent clients out there.
qBittorrent is fast, stable and provides unicode support as well as many
features.")
    (license l:gpl2+)))

(define-public deluge
  (package
    (name "deluge")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ftp.osuosl.org/pub/deluge/source/"
             (version-major+minor version) "/deluge-" version ".tar.xz"))
       (sha256
        (base32
         "1n15dzfnz1gvb4cf046yhi404i3gs933qgz0ichna6r1znmh9gf4"))))
    (build-system python-build-system)
    (inputs (list bash-minimal))
    (propagated-inputs
     (list gtk+
           libtorrent-rasterbar
           python-pycairo
           python-chardet
           python-dbus
           python-mako
           python-pygobject
           python-pillow
           python-pyopenssl
           python-pyxdg
           python-rencode
           python-service-identity
           python-setproctitle
           python-six
           python-twisted
           python-zope-interface))
    (native-inputs
     (list intltool python-wheel
           (if (string-prefix? "x86_64-" (%current-system))
               librsvg-bootstrap
               librsvg-2.40)))
    ;; TODO: Enable tests.
    ;; After "pytest-twisted" is packaged, HOME is set, and an X server is
    ;; started, some of the tests still fail.  There are likely some tests
    ;; that require a network connection.
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   ;; "librsvg" input is only needed at build time and it
                   ;; conflit with the "librsvg" propageted by "gtk+", so we
                   ;; make sure there is no reference to it in the wrapper.
                   (gi-typelib-path
                    (string-join (filter
                                  (lambda (x) (not (string-prefix?
                                                    (assoc-ref
                                                     (or native-inputs inputs)
                                                     "librsvg")
                                                    x)))
                                  (string-split
                                   (getenv "GI_TYPELIB_PATH")
                                   #\:))
                                 ":")))
               (for-each
                (lambda (program)
                  (wrap-program program
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                (map (lambda (name)
                       (string-append out "/bin/" name))
                     '("deluge" "deluge-gtk"))))
             #t)))))
    (home-page "https://www.deluge-torrent.org/")
    (synopsis  "Fully-featured cross-platform ​BitTorrent client")
    (description
     "Deluge contains the common features to BitTorrent clients such as
Protocol Encryption, DHT, Local Peer Discovery (LSD), Peer Exchange
(PEX), UPnP, NAT-PMP, Proxy support, Web seeds, global and per-torrent
speed limits.  Deluge heavily utilises the ​libtorrent library.  It is
designed to run as both a normal standalone desktop application and as a
​client-server.")
    (license l:gpl3+)))
