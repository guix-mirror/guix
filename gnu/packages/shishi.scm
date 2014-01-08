;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Nikita Karetnikov <nikita@karetnikov.org>
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
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (gnu packages)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public shishi
  (package
    (name "shishi")
    (version "1.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/shishi/shishi-"
                          version ".tar.gz"))
      (sha256
       (base32
        "032qf72cpjdfffq1yq54gz3ahgqf2ijca4vl31sfabmjzq9q370d"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("zlib" ,zlib)
       ;; libgcrypt 1.6 fails because of the following test:
       ;;  #include <gcrypt.h>
       ;; /* GCRY_MODULE_ID_USER was added in 1.4.4 and gc-libgcrypt.c
       ;;    will fail on startup if we don't have 1.4.4 or later, so
       ;;    test for it early. */
       ;; #if !defined GCRY_MODULE_ID_USER
       ;; error too old libgcrypt
       ;; #endif
       ("libgcrypt" ,libgcrypt-1.5)
       ("libtasn1" ,libtasn1)))
    (home-page "http://www.gnu.org/software/shishi/")
    (synopsis "Implementation of the Kerberos 5 network security system")
    (description
     "GNU Shishi is a free implementation of the Kerberos 5 network security
system.  It is used to allow non-secure network nodes to communicate in a
secure manner through client-server mutual authentication via tickets.")
    (license gpl3+)))
