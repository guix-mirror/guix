;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages shishi)
  #:use-module (guix licenses)
  #:use-module (distro)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages gnupg)
  #:use-module ((gnu packages compression)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public shishi
  (package
    (name "shishi")
    (version "1.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnu/shishi/shishi-"
            version
            ".tar.gz"))
      (sha256
       (base32
        "13c6w9rpaqb3am65nrn86byvmll5r78pld2vb0i68491vww4fzlx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       '("CPPFLAGS=-DMAX_ERROR_DESCRIPTION_SIZE=ASN1_MAX_ERROR_DESCRIPTION_SIZE")
       #:patches (list (assoc-ref %build-inputs
                                  "patch/gets"))))
    (inputs
     `(("gnutls" ,gnutls)
       ("zlib" ,guix:zlib)
       ("libgcrypt" ,libgcrypt)
       ("libtasn1" ,libtasn1)
       ("patch/gets" ,(search-patch "shishi-gets-undeclared.patch"))))
    (home-page "http://www.gnu.org/software/shishi/")
    (synopsis
     "GNU Shishi, free implementation of the Kerberos 5 network security system")
    (description
     " GNU Shishi is an implementation of the Kerberos 5 network
  authentication system, as specified in RFC 4120.  Shishi can be
  used to authenticate users in distributed systems.

  Shishi contains a library (`libshishi') that can be used by
  application developers to add support for Kerberos 5.  Shishi
  contains a command line utility (1shishi') that is used by
  users to acquire and manage tickets (and more).  The server
  side, a Key Distribution Center, is implemented by `shishid'.
")
    (license gpl3+))) ; some files are under GPLv2+
