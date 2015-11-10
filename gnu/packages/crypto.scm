;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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
    (version "1.0.0")
    (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://download.libsodium.org/libsodium/releases/libsodium-"
                  version ".tar.gz"))
            (sha256
             (base32
              "19f9vf0shfp4rc4l791r6xjg06z4i8psj1zkjkm3z5b640yzxlff"))))
    (build-system gnu-build-system)
    (synopsis "Portable NaCl-based crypto library")
    (description
     "Sodium is a new easy-to-use high-speed software library for network
communication, encryption, decryption, signatures, etc.")
    (license isc)
    (home-page "http://libsodium.org")))

(define-public sparsehash
  (package
    (name "sparsehash")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sparsehash/sparsehash/"
                                  "archive/sparsehash-" version ".tar.gz"))
              (sha256
               (base32
                "133szz0ldwch0kd91l0sa57qsnl020njn622szd9cxrjqyjqds85"))))
    (build-system gnu-build-system)
    (synopsis "Memory-efficient hashtable implementations")
    (description
     "This library contains several hash-map implementations, similar in API
to SGI's @code{hash_map} class, but with different performance
characteristics.  @code{sparse_hash_map} uses very little space overhead, 1-2
bits per entry.  @code{dense_hash_map} is very fast, particulary on lookup.
@code{sparse_hash_set} and @code{dense_hash_set} are the set versions of these
routines.  All these implementation use a hashtable with internal quadratic
probing.  This method is space-efficient -- there is no pointer overhead --
and time-efficient for good hash functions.")
    (home-page "https://github.com/sparsehash/sparsehash")
    (license bsd-3)))
