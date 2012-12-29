;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Andreas Enge <andreas@enge.fr>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages gsasl)
  #:use-module (distro)
  #:use-module ((distro packages compression)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (distro packages gnutls)
  #:use-module (distro packages libidn)
  #:use-module (distro packages nettle)
  #:use-module (distro packages shishi)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libntlm
  (package
   (name "libntlm")
   (version "1.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.nongnu.org/libntlm/releases/libntlm-" version
                                ".tar.gz"))
            (sha256 (base32
                     "101pr110ardcj2di940g6vaqifsaxc44h6hjn81l63dvmkj5a6ga"))))
   (build-system gnu-build-system)
   (synopsis "Libntlm, a library that implements NTLM authentication")
   (description
    "Libntlm is a library that implements NTLM authentication")
   (license lgpl2.1+)
   (home-page "http://www.nongnu.org/libntlm/")))

(define-public gss
  (package
   (name "gss")
   (version "1.0.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gss/gss-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1qa8lbkzi6ilfggx7mchfzjnchvhwi68rck3jf9j4425ncz7zsd9"))))
   (build-system gnu-build-system)
   (inputs `(("nettle" ,nettle)
             ("shishi" ,shishi)
             ("zlib" ,guix:zlib)
            ))
   (synopsis "GNU GSS (Generic Security Service), a free implementatio of RFC 2743/2744")
   (description
    "GNU GSS is an implementation of the Generic Security Service Application
Program Interface (GSS-API). GSS-API is used by network servers to provide
security services, e.g., to authenticate SMTP/IMAP clients against
SMTP/IMAP servers. GSS consists of a library and a manual.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/gss/")))

(define-public gsasl
  (package
   (name "gsasl")
   (version "1.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gsasl/gsasl-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1rci64cxvcfr8xcjpqc4inpfq7aw4snnsbf5xz7d30nhvv8n40ii"))))
   (build-system gnu-build-system)
   (inputs `(("libidn" ,libidn)
             ("libntlm" ,libntlm)
             ("gnutls" ,gnutls)
             ("gss" ,gss)
             ("zlib" ,guix:zlib)
             ))
   (synopsis "GNU SASL, an implementation of the Simple Authentication and Security Layer framework")
   (description
    "GNU SASL is an implementation of the Simple Authentication and Security
Layer framework and a few common SASL mechanisms. SASL is used by network
servers (e.g., IMAP, SMTP) to request authentication from clients, and in
clients to authenticate against servers.

GNU SASL consists of a library (libgsasl), a command line utility (gsasl)
to access the library from the shell, and a manual. The library includes
support for the framework (with authentication functions and application
data privacy and integrity functions) and at least partial support for the
CRAM-MD5, EXTERNAL, GSSAPI, ANONYMOUS, PLAIN, SECURID, DIGEST-MD5,
SCRAM-SHA-1, SCRAM-SHA-1-PLUS, LOGIN, and NTLM mechanisms.

The library is portable because it does not do network communication by
itself, but rather leaves it up to the calling application. The library is
flexible with regards to the authorization infrastructure used, as it
utilises callbacks into the application to decide whether an user is
authorised or not.

The gsasl package distribution includes the library part as well,
so there is no need to install two packages.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/gsasl/")))
