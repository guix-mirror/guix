;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages aria2)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config))

(define-public c-ares
  (package
    (name "c-ares")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://c-ares.haxx.se/download/" name "-" version
                    ".tar.gz"))
              (sha256
               (base32
                "1nyka87yf2jfd0y6sspll0yxwb8zi7kyvajrdbjmh4axc5s1cw1x"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://c-ares.haxx.se/")
    (synopsis "C library for asynchronous DNS requests")
    (description
      "C-ares is a C library that performs DNS requests and name resolution
asynchronously.  It is intended for applications which need to perform DNS
queries without blocking, or need to perform multiple DNS queries in parallel.
The primary examples of such applications are servers which communicate with
multiple clients and programs with graphical user interfaces.")
    (license (license:x11-style "http://c-ares.haxx.se/license.html"))))
