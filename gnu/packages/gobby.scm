;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Andy Wingo <wingo@igalia.com>
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
