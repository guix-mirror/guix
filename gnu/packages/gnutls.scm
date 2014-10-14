;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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
  #:use-module ((gnu packages compression) #:prefix guix:)
  #:use-module (gnu packages)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages which)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (srfi srfi-1))

(define-public libtasn1
  (package
    (name "libtasn1")
    (version "4.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/libtasn1/libtasn1-"
                          version ".tar.gz"))
      (sha256
       (base32
        "00gkyppzw6fqi5mnc3d8paf7bp6nfhi9213481awy07sviqnbvk0"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)

                     ;; XXX: For some reason, libtasn1.info wants to be
                     ;; rebuilt, so we must provide 'makeinfo'.
                     ("texinfo" ,texinfo)))
    (home-page "http://www.gnu.org/software/libtasn1/")
    (synopsis "ASN.1 library")
    (description
     "GNU libtasn1 is a library implementing the ASN.1 notation.  It is used
for transmitting machine-neutral encodings of data objects in computer
networking, allowing for formal validation of data according to some
specifications.")
    (license lgpl2.0+)))

(define-public gnutls
  (package
    (name "gnutls")
    (version "3.2.19")
    (source (origin
             (method url-fetch)
             (uri
              ;; Note: Releases are no longer on ftp.gnu.org since the
              ;; schism (after version 3.1.5).
              (string-append "mirror://gnupg/gnutls/v"
                             (string-join (take (string-split version #\.) 2)
                                          ".")
                             "/gnutls-" version ".tar.xz"))
             (sha256
              (base32
               "1krx33ab2ijwfz71f1ba8labxfsic7jhlhv6rvjsyw566jj9a3d2"))
             (patches (list (search-patch "gnutls-server-name-fix.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.0)
       ("zlib" ,guix:zlib)
       ("perl" ,perl)))
    (propagated-inputs
     `(("libtasn1" ,libtasn1)
       ("nettle" ,nettle)
       ("which" ,which)))
    (home-page "http://www.gnu.org/software/gnutls/")
    (synopsis "Transport layer security library")
    (description
     "GnuTLS is a secure communications library implementing the SSL, TLS
and DTLS protocols.  It is provided in the form of a C library to support the
protocols, as well as to parse and write X.5009, PKCS 12, OpenPGP and other
required structures.")
    (license lgpl2.1+)))
