;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tls)
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
    (version "18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aperezdc/signify/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00lbjiy0gv1b1fvrd6ni4qah96ah4qf6aig05vd2hji4vs00jgwg"))))
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


(define-public opendht
  (package
    (name "opendht")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/savoirfairelinux/" name
         "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "src/argon2")
           (substitute* "src/Makefile.am"
             (("./argon2/libargon2.la") "")
             (("SUBDIRS = argon2") ""))
           (substitute* "src/crypto.cpp"
             (("argon2/argon2.h") "argon2.h"))
           (substitute* "configure.ac"
             (("src/argon2/Makefile") ""))))
       (sha256
        (base32
         "09yvkmbqbym3b5md4n96qc1s9sf2n8ji404hagih45rmsj49599x"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("nettle" ,nettle)
       ("msgpack" ,msgpack)
       ("readline" ,readline)
       ("argon2" ,argon2)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("pkg-config" ,pkg-config)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     `(#:configure-flags '("--disable-tools" "--disable-python")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'autoconf
                    (lambda _
                      (zero? (system* "autoreconf" "-vfi")))))))
    (home-page "https://github.com/savoirfairelinux/opendht/")
    (synopsis "Distributed Hash Table (DHT) library")
    (description "OpenDHT is a Distributed Hash Table (DHT) library.  It may
be used to manage peer-to-peer network connections as needed for real time
communication.")
    (license gpl3)))
