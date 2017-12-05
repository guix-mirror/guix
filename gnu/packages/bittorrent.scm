;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
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
  #:use-module (gnu packages databases)
  #:use-module (gnu packages file)
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
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public transmission
  (package
    (name "transmission")
    (version "2.92")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://transmission.cachefly.net/transmission-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0pykmhi7pdmzq47glbj8i2im6iarp4wnj4l1pyvsrnba61f0939s"))))
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
  (let ((commit "9755b50e9444566cff02c977edafdbb3e9750cbb")
        (revision "1"))
    (package
      (name "tremc")
      (version (git-version "0.9.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/louipc/tremc.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "05259qss5jka5ygwrh7cngyp6cgazbynji5pshgfzrd2d43pyfq5"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; no test suite
         #:phases
         (modify-phases %standard-phases
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
                 (install-file "tremc" bin)
                 (install-file "tremc.1" man)
                 (install-file
                   (string-append
                     "completion/bash/"
                     "transmission-remote-cli-bash-completion.sh")
                   completions)))))))
      (synopsis "Console client for the Transmission BitTorrent daemon")
      (description "Tremc is a console client, with a curses interface, for the
Transmission BitTorrent daemon.")
      (home-page "https://github.com/louipc/tremc")
      (license l:gpl3+))))

(define-public transmission-remote-cli
  (package
    (name "transmission-remote-cli")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fagga/"
                                  "transmission-remote-cli/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y0hkpcjf6jw9xig8yf484hbhy63nip0pkchx401yxj81m25l4z9"))))
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
    (version "1.33.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tatsuhiro-t/aria2/"
                                  "releases/download/release-" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "06syqxia701dk96rcbhnd4x0arjj6d22gm3aqksz38am9y2f8f95"))))
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
    (native-inputs
     `(("intltool" ,intltool)))
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
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mktorrent/mktorrent/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "17qi3nfky240pq6qcmf5qg324mxm83vk9r3nvsdhsvinyqm5d3kg"))))
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
    (home-page "http://mktorrent.sourceforge.net/")
    (synopsis "Utility to create BitTorrent metainfo files")
    (description "mktorrent is a simple command-line utility to create
BitTorrent @dfn{metainfo} files, often known simply as @dfn{torrents}, from
both single files and whole directories.  It can add multiple trackers and web
seed URLs, and set the @code{private} flag to disallow advertisement through
the distributed hash table (DHT) and Peer Exchange.  Hashing is multi-threaded
and will take advantage of multiple processor cores where possible.")
    (license (list l:public-domain      ; sha1.*, used to build without OpenSSL
                   l:gpl2+))))          ; with permission to link with OpenSSL

(define-public libtorrent-rasterbar
  (package
    (name "libtorrent-rasterbar")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/arvidn/libtorrent/releases/download/libtorrent-"
                (string-join (string-split version #\.) "_")
                "/libtorrent-rasterbar-" version ".tar.gz"))
              (sha256
               (base32
                "0c398b7hsa5dvj4m0jc8h7mn0m3nawmagb6c5c7ml5c9hc338c8h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir="
                            (assoc-ref %build-inputs "boost")
                            "/lib")
             "--enable-python-binding"
             "--enable-tests")
       #:make-flags (list
                     (string-append "LDFLAGS=-Wl,-rpath="
                                    (assoc-ref %outputs "out") "/lib"))))
    (inputs `(("boost" ,boost)
              ("openssl" ,openssl)))
    (native-inputs `(("python" ,python-2)
                     ("pkg-config" ,pkg-config)))
    (home-page "http://www.rasterbar.com/products/libtorrent/")
    (synopsis "Feature complete BitTorrent implementation")
    (description
     "libtorrent-rasterbar is a feature complete C++ BitTorrent implementation
focusing on efficiency and scalability.  It runs on embedded devices as well as
desktops.")
    (license l:bsd-2)))


