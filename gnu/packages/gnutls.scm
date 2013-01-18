;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gnutls)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages compression)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages which))

(define-public libtasn1
  (package
    (name "libtasn1")
    (version "3.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/libtasn1/libtasn1-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0gvgndypwicchf7m660zh7jdgmkfj9g9xavpcc08pyd0120y0bk7"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/libtasn1/")
    (synopsis "GNU Libtasn1, an ASN.1 library")
    (description
     "Libtasn1 is the ASN.1 library used by GnuTLS, GNU Shishi and some
other packages.  The goal of this implementation is to be highly
portable, and only require an ANSI C89 platform.")
    (license lgpl2.0+)))

(define-public gnutls
  (package
    (name "gnutls")
    (version "3.1.6")
    (source (origin
             (method url-fetch)
             (uri
              ;; Note: Releases are no longer on ftp.gnu.org since the
              ;; schism (after version 3.1.5).
              (string-append "mirror://gnupg/gnutls/v3.1/gnutls-"
                             version ".tar.xz"))
             (sha256
              (base32
               "0zsybr9plllk1phh83bx9bg7c5ccik427j4n3k1s9fiy4j69n0w3"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-2.0)
       ("zlib" ,guix:zlib)
       ("perl" ,perl)))
    (propagated-inputs
     `(("libtasn1" ,libtasn1)
       ("nettle" ,nettle)
       ("which" ,which)))
    (home-page "http://www.gnu.org/software/gnutls/")
    (synopsis "The GNU Transport Layer Security Library")
    (description
     "GnuTLS is a project that aims to develop a library which provides
a secure layer, over a reliable transport layer. Currently the GnuTLS
library implements the proposed standards by the IETF's TLS working
group.

Quoting from the TLS protocol specification:

\"The TLS protocol provides communications privacy over the
Internet. The protocol allows client/server applications to communicate
in a way that is designed to prevent eavesdropping, tampering, or
message forgery.\"")
    (license lgpl2.1+)))
