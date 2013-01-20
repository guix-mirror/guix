;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages tor)
  #:use-module ((guix licenses) #:select (bsd-3))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages openssl))

(define-public tor
  (package
    (name "tor")
    (version "0.2.3.25")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.torproject.org/dist/tor-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "183n667zgmp37brxl4qna1ciw6dz8d0ihd3fgv9i2gpk6q8nybdv"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("openssl" ,openssl)
       ("libevent" ,libevent)))
    (home-page "http://www.torproject.org/")
    (synopsis "An anonymous network router to improve privacy on the Internet")
    (description
     "Tor protects you by bouncing your communications around a distributed
network of relays run by volunteers all around the world: it prevents
somebody watching your Internet connection from learning what sites you
visit, and it prevents the sites you visit from learning your physical
location. Tor works with many of your existing applications, including
web browsers, instant messaging clients, remote login, and other
applications based on the TCP protocol.")
    (license bsd-3)))
