;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

(define-module (gnu packages crypto)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libsodium
  (package
    (name "libsodium")
    (version "0.5.0")
    (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://download.libsodium.org/libsodium/releases/libsodium-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1w7rrnsvhhzhywrr3nhlhppv4kqzdszz3dwy8jrsv8lrj5hs181w"))))
    (build-system gnu-build-system)
    (synopsis "Portable NaCl-based crypto library")
    (description
     "libsodium is a new easy-to-use high-speed software library for network
communication, encryption, decryption, signatures, etc.")
    (license isc)
    (home-page "https://github.com/jedisct1/libsodium"))) ; No real homepage
