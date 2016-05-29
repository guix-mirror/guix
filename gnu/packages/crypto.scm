;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libbsd)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libsodium
  (package
    (name "libsodium")
    (version "1.0.10")
    (source (origin
            (method url-fetch)
            (uri (list (string-append
                        "http://download.libsodium.org/libsodium/"
                        "releases/libsodium-" version ".tar.gz")
                       (string-append
                        "https://download.libsodium.org/libsodium/"
                        "releases/old/libsodium-" version ".tar.gz")))
            (sha256
             (base32
              "1gn45g956lyz8l6iq187yc6l627vyivyp8qc5dkr6dnhdnlqddvi"))))
    (build-system gnu-build-system)
    (synopsis "Portable NaCl-based crypto library")
    (description
     "Sodium is a new easy-to-use high-speed software library for network
communication, encryption, decryption, signatures, etc.")
    (license isc)
    (home-page "http://libsodium.org")))

(define-public signify
  (package
    (name "signify")
    (version "17")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aperezdc/signify/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kfv2k1fqck31vwlnicavb0h541ilad9zd7j8zz8x2kx36wwqpr7"))))
    (build-system gnu-build-system)
    ;; TODO Build with libwaive (described in README.md), to implement something
    ;; like OpenBSD's pledge().
    (arguments
     `(#:tests? #f ; no test suite
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libbsd" ,libbsd)))
    (synopsis "Create and verify cryptographic signatures")
    (description "The signify utility creates and verifies cryptographic
signatures using the elliptic curve Ed25519.  This is a Linux port of the
OpenBSD tool of the same name.")
    (home-page "https://github.com/aperezdc/signify")
    ;; This package includes third-party code that was originally released under
    ;; various non-copyleft licenses. See the source files for clarification.
    (license (list bsd-3 bsd-4 expat isc public-domain
                   (non-copyleft "file://base64.c"
                                 "See base64.c in the distribution for
                                 the license from IBM.")))))
