;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages file)
  #:use-module (gnu packages linux)
  #:use-module ((gnu packages compression)
                #:select (zlib))
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages check)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl))

(define-public transmission
  (package
    (name "transmission")
    (version "2.84")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://transmission.cachefly.net/transmission-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1sxr1magqb5s26yvr5yhs1f7bmir8gl09niafg64lhgfnhv1kz59"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out"                      ; library and command-line interface
               "gui"))                    ; graphical user interface
    (arguments
     '(#:glib-or-gtk-wrap-excluded-outputs '("out")
       #:phases (alist-cons-after
                 'install 'move-gui
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Move the GUI to its own output, so that "out" doesn't
                   ;; depend on GTK+.
                   (let ((out (assoc-ref outputs "out"))
                         (gui (assoc-ref outputs "gui")))
                     (mkdir-p (string-append gui "/bin"))
                     (rename-file (string-append out "/bin/transmission-gtk")
                                  (string-append gui
                                                 "/bin/transmission-gtk"))))
                 %standard-phases)))
    (inputs
     `(("inotify-tools" ,inotify-tools)
       ("libevent" ,libevent)
       ("curl" ,curl)
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
unattended operationg.  It supports local peer discovery, full encryption,
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
    (version "0.13.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://libtorrent.rakshasa.no/downloads/libtorrent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0ma910br5vxrfpm4f4w4942lpmhwvqjnnf9h8vpf52fw35qhjkkh"))))
    (build-system gnu-build-system)
    (inputs `(("openssl" ,openssl)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ;; Add this when you enable tests:
                     ;; ("cppunit" ,cppunit)
                     ))
    (arguments
     ;; FIXME: enable tests on the next release:
     ;; https://github.com/rakshasa/libtorrent/issues/59
     `(#:tests? #f))
    (synopsis "BitTorrent library of rtorrent")
    (description
     "LibTorrent is a BitTorrent library used by and developed in parallel
with the BitTorrent client rtorrent.  It is written in C++ with emphasis on
speed and efficiency.")
    (home-page "http://libtorrent.rakshasa.no/")
    (license l:gpl2+)))

(define-public rtorrent
  (package
    (name "rtorrent")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://libtorrent.rakshasa.no/downloads/rtorrent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "113yrrac75vqi4g8r6bgs0ggjllj9bkg9shv08vqzdhkwqg2q2mw"))))
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
    (home-page "http://libtorrent.rakshasa.no/")
    (license l:gpl2+)))
