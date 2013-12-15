;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages curl)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh))

(define-public curl
  (package
   (name "curl")
   (version "7.28.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://curl.haxx.se/download/curl-"
                                version ".tar.lzma"))
            (sha256
             (base32
              "13bhfs41yf60ys2hrikqxjwfzaj0gm91kqzsgc5fr4grzmpm38nx"))))
   (build-system gnu-build-system)
   (inputs `(("gnutls" ,gnutls)
             ("gss" ,gss)
             ("libidn" ,libidn)
             ("libssh2" ,libssh2)
             ("openldap" ,openldap)
             ("zlib" ,zlib)))
   (native-inputs
     `(("perl" ,perl)
       ;; to enable the --manual option and make test 1026 pass
       ("groff" ,groff)
       ("pkg-config" ,pkg-config)))
   (arguments
    `(#:configure-flags '("--with-gnutls" "--with-gssapi")))
   (synopsis "curl, command line tool for transferring data with URL syntax")
   (description
    "curl is a command line tool for transferring data with URL syntax,
supporting DICT, FILE, FTP, FTPS, Gopher, HTTP, HTTPS, IMAP, IMAPS, LDAP,
LDAPS, POP3, POP3S, RTMP, RTSP, SCP, SFTP, SMTP, SMTPS, Telnet and TFTP.
curl supports SSL certificates, HTTP POST, HTTP PUT, FTP uploading,
HTTP form based upload, proxies, cookies, user+password authentication
(Basic, Digest, NTLM, Negotiate, kerberos...), file transfer resume,
proxy tunneling, and so on")
   (license (license:bsd-style "file://COPYING"
                       "See COPYING in the distribution."))
   (home-page "http://curl.haxx.se/")))
