;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
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

(define-module (gnu packages jose)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls))

(define-public jose
  (package
    (name "jose")
    (version "10")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/latchset/jose/releases/download/v10/jose-"
                              version ".tar.bz2"))
              (sha256
               (base32
                "0wndxz3jqxfxnv5396da3kc1say7442m7mwk2dw9ykawagxxr72w"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("jansson" ,jansson)
              ("zlib" ,zlib)
              ("libcrypto" ,openssl)))
    (build-system gnu-build-system)
    (home-page "https://github.com/latchset/jose")
    (synopsis "Object Signing and Encryption")
    (description "C-language implementation of Javascript Object Signing and
Encryption")
    (license license:asl2.0)))
