;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages adns)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config))

(define-public adns
  (package
    (name "adns")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "mirror://gnu/adns/adns-"
                                        version ".tar.gz")
                         (string-append
                          "http://www.chiark.greenend.org.uk/~ian/adns/ftp/adns-"
                          version ".tar.gz")))
              (sha256
               (base32
                "1ssfh94ck6kn98nf2yy6743srpgqgd167va5ja3bwx42igqjc42v"))))
    (build-system gnu-build-system)
    (arguments
     ;; Make sure the programs under bin/ fine libadns.so.
     '(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath -Wl,"
                                              (assoc-ref %outputs "out")
                                              "/lib"))

       ;; XXX: Tests expect real name resolution to work.
       #:tests? #f))
    (home-page "https://www.gnu.org/software/adns/")
    (synopsis "Asynchronous DNS client library and utilities")
    (description
     "GNU adns is a C library that provides easy-to-use DNS resolution
functionality.  The library is asynchronous, allowing several concurrent
calls.  The package also includes several command-line utilities for use in
scripts.")
    (license gpl3+)))

(define-public c-ares
  (package
    (name "c-ares")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://c-ares.haxx.se/download/" name "-" version
                    ".tar.gz"))
              (sha256
               (base32
                "0lk8knip4xk6qzksdkn7085mmgm4ixfczdyyjw656c193y3rgnvc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://c-ares.haxx.se/")
    (synopsis "C library for asynchronous DNS requests")
    (description
      "C-ares is a C library that performs DNS requests and name resolution
asynchronously.  It is intended for applications which need to perform DNS
queries without blocking, or need to perform multiple DNS queries in parallel.
The primary examples of such applications are servers which communicate with
multiple clients and programs with graphical user interfaces.")
    (license (x11-style "https://c-ares.haxx.se/license.html"))))
