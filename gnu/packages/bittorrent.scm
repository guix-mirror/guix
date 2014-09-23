;;; GNU Guix --- Functional package management for GNU
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
  #:use-module (gnu packages gtk))

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
    (build-system gnu-build-system)
    (outputs '("out"                      ; library and command-line interface
               "gui"))                    ; graphical user interface
    (arguments
     '(#:phases (alist-cons-after
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
