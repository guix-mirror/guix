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

(define-module (gnu packages gvpe)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages openssl)
  #:use-module ((gnu packages compression) #:select (zlib)))

(define-public gvpe
  (package
    (name "gvpe")
    (version "2.25")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gvpe/gvpe-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1gsipcysvsk80gvyn9jnk9g0xg4ng9yd5zp066jnmpgs52d2vhvk"))))
    (build-system gnu-build-system)
    (home-page "http://software.schmorp.de/pkg/gvpe.html")
    (inputs `(("openssl" ,openssl)
              ("zlib" ,zlib)))
    (synopsis "Secure VPN among multiple nodes over an untrusted network")
    (description
     "The GNU Virtual Private Ethernet creates a virtual network
with multiple nodes using a variety of transport protocols.  It works
by creating encrypted host-to-host tunnels between multiple
endpoints.")
    (license gpl3+)))
