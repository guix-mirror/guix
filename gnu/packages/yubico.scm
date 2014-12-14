;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages yubico)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages man)
  #:use-module (gnu packages curl))

(define-public libyubikey
  (package
    (name "libyubikey")
    (version "1.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubico-c/Releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "1f0plzmr1gwry4rfgq9q70v6qwqny009hac289ad5m6sj7vqflxr"))))
    (build-system gnu-build-system)
    (synopsis "Development kit for the YubiKey authentication device")
    (description
     "This package contains a C library and command-line tools that make up
the low-level development kit for the Yubico YubiKey authentication device.")
    (home-page "https://developers.yubico.com/yubico-c/")
    (license bsd-2)))

(define-public ykclient
  (package
    (name "ykclient")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubico-c-client/Releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lw1j61rfjngs8vvv9m348zl4166zg24bq0dy72r44wiz79yic4j"))))
    (build-system gnu-build-system)

    ;; There's just one test, and it requires network access to access
    ;; yubico.com, so skip it.
    (arguments '(#:tests? #f))

    (native-inputs `(("pkg-config" ,pkg-config)
                     ("help2man" ,help2man)))
    (inputs `(("curl" ,curl)))
    (synopsis "C library to validate one-time-password YubiKeys")
    (description
     "YubiKey C Client Library (libykclient) is a C library used to validate a
one-time-password (OTP) YubiKey against Yubico’s servers.  See the Yubico
website for more information about Yubico and the YubiKey.")
    (home-page "https://developers.yubico.com/yubico-c-client/")
    (license bsd-2)))
