;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017, 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages gobby)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public libnet6
  (package
    (name "libnet6")
    (version "1.3.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://releases.0x539.de/net6/net6-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "088yqq60wjx3jqjlhl12893p15gl9asjpavrbhh590qzpqndhp8m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       '("CXXFLAGS=-std=c++11")         ; required by libsigc++
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-gnutls-api
           (lambda _
             (substitute* "src/encrypt.cpp"
               ;; The GnuTLS API to set authentication and other parameters
               ;; and priorities changed in 3.4; update to allow ANON_DH via
               ;; the new API.
               (("gnutls_kx_set_priority\\(session, kx_prio\\)")
                (string-append "gnutls_priority_set_direct"
                               "(session, \"NORMAL:+ANON-DH\", NULL)"))))))))
    (inputs
     `(("libsigc++" ,libsigc++)
       ("gnutls" ,gnutls)))
    (home-page "https://gobby.github.io/")
    (synopsis "Network access framework for IPv4/IPv6")
    (description
     "Library which that provides a TCP protocol abstraction for C++.")
    (license license:lgpl2.1)))

(define-public obby
  (package
    (name "obby")
    (version "0.4.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://releases.0x539.de/obby/obby-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rwvp0kzsb8y6mq73rzb8yk4kvsrz64i2zf4lfqs3kh0x2k7n7bx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libsigc++" ,libsigc++)
       ("gnutls" ,gnutls)
       ("libnet6" ,libnet6)))
    (arguments
     ;; Required by libsigc++.
     `(#:configure-flags '("CXXFLAGS=-std=c++11")))
    (home-page "https://gobby.github.io/")
    (synopsis "Library for building collaborative editors")
    (description
     "Library that provides synced document buffers.  It supports multiple
documents in one session.  Obby is used by the Gobby collaborative editor.")
    (license license:gpl2+)))

;; Although there is a newer version of Gobby defined below, the protocols are
;; incompatible; you need Gobby 0.4 if you want to connect to servers running
;; the 0.4 protocol.
(define-public gobby-0.4
  (package
    (name "gobby")
    (version "0.4.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://releases.0x539.de/gobby/gobby-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0w8q01lf6bcdz537b29m7rwlbc7k87b12vnpm1h6219ypvzqkgcc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("libxml++-2" ,libxml++-2)
       ("gnutls" ,gnutls)
       ("gtkmm-2" ,gtkmm-2)
       ("gtksourceview-2" ,gtksourceview-2)
       ("libnet6" ,libnet6)
       ("obby" ,obby)))
    (arguments
     ;; Required by libsigc++.
     `(#:configure-flags '("CXXFLAGS=-std=c++11")))
    (home-page "https://gobby.github.io/")
    (synopsis "Collaborative editor")
    (description
     "Collaborative editor that supports multiple documents in one session and
a multi-user chat.  Gobby allows multiple users to edit the same document
together over the internet in real-time.

This is the older 0.4 version of Gobby.  Use this version only if you need to
connect to a server running the old 0.4 protocol.")
    (license license:gpl2+)))

(define-public gobby
  (package
    (name "gobby")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://releases.0x539.de/gobby/gobby-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "165x0r668ma5blziisvbr8qig3jw9hf7i6w8r7wwvz3wsac3bswc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gnutls" ,gnutls)
       ("gsasl" ,gsasl)
       ("gtkmm-2" ,gtkmm-2)
       ("gtksourceview-2" ,gtksourceview-2)
       ("libinfinity" ,libinfinity)
       ("libxml++-2" ,libxml++-2)))
    (arguments
     ;; Required by libsigc++.
     `(#:configure-flags '("CXXFLAGS=-std=c++11")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion (assoc-ref outputs "out")
               (rename-file "bin/gobby-0.5" "bin/gobby"))
             #t)))))
    (home-page "https://gobby.github.io/")
    (synopsis "Collaborative editor")
    (description
     "Collaborative editor that supports multiple documents in one session and
a multi-user chat.  Gobby allows multiple users to edit the same document
together over the internet in real-time.")
    (license license:gpl2+)))

(define-public libinfinity
  (package
    (name "libinfinity")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://releases.0x539.de/libinfinity/libinfinity-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jw2fhrcbpyz99bij07iyhy9ffyqdn87vl8cb1qz897y3f2f0vk2"))))
    (build-system gnu-build-system)
    (inputs
     `(("glib" ,glib)
       ("gsasl" ,gsasl)
       ("gtk+" ,gtk+)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags (list "--with-inftextgtk"
                               "--with-infgtk")))
    (home-page "https://gobby.github.io/")
    (synopsis "Infininote protocol implementation")
    (description "libinfinity is a library to build collaborative text
editors.  Changes to the text buffers are synced to all other clients over a
central server.  Even though a central server is involved, the local user sees
his changes applied instantly and the merging is done on the individual
clients.")
    (license license:lgpl2.1+)))
