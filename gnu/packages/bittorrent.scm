;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public transmission
  (package
    (name "transmission")
    (version "2.94")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/transmission/transmission-releases/raw/"
                    "master/transmission-" version ".tar.xz"))
              (sha256
               (base32
                "0zbbj7rlm6m7vb64x68a64cwmijhsrwx9l63hbwqs7zr9742qi1m"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out"                      ; library and command-line interface
               "gui"))                    ; graphical user interface
    (arguments
     '(#:glib-or-gtk-wrap-excluded-outputs '("out")
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
                            (string-append gui
                                           "/bin/transmission-gtk"))

               ;; Move the '.desktop' file as well.
               (mkdir (string-append gui "/share"))
               (rename-file (string-append out "/share/applications")
                            (string-append gui "/share/applications")))
             #t)))))
    (inputs
     `(("inotify-tools" ,inotify-tools)
       ("libevent" ,libevent)
       ("curl" ,curl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("openssl" ,openssl)
       ("file" ,file)
       ("zlib" ,zlib)
       ("gtk+" ,gtk+)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.transmissionbt.com/")
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
    (license l:gpl3+)))

(define-public libtorrent
  (package
    (name "libtorrent")
    (version "0.13.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://rtorrent.net/downloads/libtorrent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "012s1nwcvz5m5r4d2z9klgy2n34kpgn9kgwgzxm97zgdjs6a0f18"))))
    (build-system gnu-build-system)
    (inputs `(("openssl" ,openssl)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cppunit" ,cppunit)))
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
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://rtorrent.net/downloads/rtorrent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "03jvzw9pi2mhcm913h8qg0qw9gwjqc6lhwynb1yz1y163x7w4s8y"))))
    (build-system gnu-build-system)
    (inputs `(("libtorrent" ,libtorrent)
              ("ncurses" ,ncurses)
              ("curl" ,curl)
              ("cyrus-sasl" ,cyrus-sasl)
              ("openssl" ,openssl)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("cppunit" ,cppunit)))
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
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tremc/tremc.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yhwvlcyv1s830p5a7q5x3mkb3mbvr5cn5nh7y62l5b6iyyynlvm"))))
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
     `(("python" ,python)))
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
                    (url "https://github.com/fagga/transmission-remote-cli.git")
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
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tatsuhiro-t/aria2/"
                                  "releases/download/release-" version "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "aria2-CVE-2019-3500.patch"))
              (sha256
               (base32
                "18vpgr430vxlwbcc3598rr1srfmwypls6wp1m4wf21hncc1ahi1s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-libaria2"
                               (string-append "--with-bashcompletiondir="
                                              %output "/etc/bash_completion.d/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-socket-tests
           (lambda _
             (substitute* "test/LpdMessageDispatcherTest.cc"
               (("CPPUNIT_TEST_SUITE_REGISTRATION\\(LpdMessageDispatcherTest\\);" text)
                (string-append "// " text)))
             (substitute* "test/LpdMessageReceiverTest.cc"
               (("CPPUNIT_TEST_SUITE_REGISTRATION\\(LpdMessageReceiverTest\\);" text)
                (string-append "// " text)))
             #t)))))
    (native-inputs
     `(("cppunit" ,cppunit) ; for the tests
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("c-ares" ,c-ares)
       ("gnutls" ,gnutls)
       ("gmp" ,gmp)
       ("libssh2" ,libssh2)
       ("libxml2" ,libxml2)
       ("nettle" ,nettle)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (home-page "https://aria2.github.io/")
    (synopsis "Utility for parallel downloading files")
    (description
      "Aria2 is a lightweight, multi-protocol & multi-source command-line
download utility.  It supports HTTP/HTTPS, FTP, SFTP, BitTorrent and Metalink.
Aria2 can be manipulated via built-in JSON-RPC and XML-RPC interfaces.")
    (license l:gpl2+)))

(define-public uget
  (package
    (name "uget")
    (version "2.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/urlget/"
                                  "uget%20%28stable%29/" version "/uget-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0919cf7lfk1djdl003cahqjvafdliv7v2l8r5wg95n4isqggdk75"))))
    (build-system gnu-build-system)
    (inputs
     `(("curl" ,curl)
       ("gtk+" ,gtk+)
       ("glib" ,glib)
       ("gnutls" ,gnutls)
       ("gstreamer" ,gstreamer)
       ("libgcrypt" ,libgcrypt)
       ("libnotify" ,libnotify)
       ("openssl" ,openssl)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://ugetdm.com/")
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
                    (url "https://github.com/Rudde/mktorrent.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17pdc5mandl739f8q26n5is8ga56s83aqcrwhlnnplbxwx2inidr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))          ; no configure script
       #:make-flags (list "CC=gcc"
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
    (version "1.1.11")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/arvidn/libtorrent/releases/download/libtorrent_"
                (string-join (string-split version #\.) "_")
                "/libtorrent-rasterbar-" version ".tar.gz"))
              (sha256
               (base32
                "0isqidr11fnhybr0wvk0qxd97jaikmh8fx9h89b84yd2gyxdw8vw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir="
                            (assoc-ref %build-inputs "boost")
                            "/lib")
             "--enable-python-binding"
             "--enable-tests"
             "CXXFLAGS=-std=c++11")     ; Use std::chrono instead of boost
       #:make-flags (list
                     (string-append "LDFLAGS=-Wl,-rpath="
                                    (assoc-ref %outputs "out") "/lib"))
       #:phases (modify-phases %standard-phases
           (add-after 'unpack 'compile-python-c++11
             (lambda _
               ;; Make sure the Python bindings are compiled in C++ mode to
               ;; avoid undefined references as mentioned in
               ;; <https://github.com/qbittorrent/qBittorrent/issues/638>.
               ;; XXX: This can be removed for 1.2+.
               (substitute* "bindings/python/setup.py"
                 (("\\+ target_specific\\(\\)\\,")
                  "+ target_specific() + ['-std=c++11'],"))
               #t)))))
    (inputs `(("boost" ,boost)
              ("openssl" ,openssl)))
    (native-inputs `(("python" ,python-2)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://www.libtorrent.org/")
    (synopsis "Feature complete BitTorrent implementation")
    (description
     "libtorrent-rasterbar is a feature complete C++ BitTorrent implementation
focusing on efficiency and scalability.  It runs on embedded devices as well as
desktops.")
    (license l:bsd-2)))

(define-public qbittorrent
  (package
    (name "qbittorrent")
    (version "4.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/qbittorrent/qBittorrent.git")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09zcygaxfv9g6av0vsvlyzv4v65wvj766xyfx31yz5ig3xan6ak1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir="
                            (assoc-ref %build-inputs "boost")
                            "/lib")
             "--enable-debug"
             "QMAKE_LRELEASE=lrelease")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("boost" ,boost)
       ("libtorrent-rasterbar" ,libtorrent-rasterbar)
       ("openssl" ,openssl)
       ("python" ,python-wrapper)
       ("qtbase" ,qtbase)
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
    (version "1.3.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://download.deluge-torrent.org/source/deluge-"
             version ".tar.xz"))
       (sha256
        (base32
         "0b7rri4x0wrcj7rjghrnw1kfrsd5i7i6aq85dsg5dg1w1qa0ar59"))))
    (build-system python-build-system)
    (inputs
     `(("libtorrent" ,libtorrent-rasterbar)
       ("python2-chardet" ,python2-chardet)
       ("python2-pygtk" ,python2-pygtk)
       ("python2-pyopenssl" ,python2-pyopenssl)
       ("python2-pyxdg" ,python2-pyxdg)
       ("python2-service-identity" ,python2-service-identity)
       ("python2-twisted" ,python2-twisted)))
    (native-inputs
     `(("intltool" ,intltool)))
    (arguments
     `(#:python ,python-2))
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
