;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages gsasl)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libntlm
  (package
    (name "libntlm")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.nongnu.org/libntlm/releases/"
                                  "libntlm-" version ".tar.gz"))
              (sha256
               (base32
                "1gcvv7f9rggpxay81qv6kw5hr6gd4qiyzkbwhzz02fx9jvv9kmsk"))))
    (build-system gnu-build-system)
    (synopsis "Library that implements NTLM authentication")
    (description
     "Libntlm is a library that implements NTLM authentication.")
    (license license:lgpl2.1+)
    (home-page "https://www.nongnu.org/libntlm/")))

(define-public gss
  (package
   (name "gss")
   (version "1.0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gss/gss-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1syyvh3k659xf1hdv9pilnnhbbhs6vfapayp4xgdcc8mfgf9v4gz"))))
   (build-system gnu-build-system)
   (inputs `(("nettle" ,nettle)
             ("shishi" ,shishi)
             ("zlib" ,zlib)))
   (synopsis "Generic Security Service library")
   (description
    "The GNU Generic Security Service provides a free implementation of the
GSS-API specification.  It provides a generic application programming
interface for programs to access security services.  Security services present
a generic, GSS interface, with which the calling application interacts via
this library, freeing the application developer from needing to know about
the underlying security implementation.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/gss/")))

(define-public gsasl
  (package
   (name "gsasl")
   (version "1.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gsasl/gsasl-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1rci64cxvcfr8xcjpqc4inpfq7aw4snnsbf5xz7d30nhvv8n40ii"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                ;; The gnulib test-lock test is prone to writer starvation
                ;; with our glibc@2.25, which prefers readers, so disable it.
                ;; The gnulib commit b20e8afb0b2 should fix this once
                ;; incorporated here.
                (substitute* "tests/Makefile.in"
                  (("test-lock\\$\\(EXEEXT\\) ") ""))
                #t))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags '("--with-gssapi-impl=mit")))
   (inputs `(("libidn" ,libidn)
             ("libntlm" ,libntlm)
             ("mit-krb5" ,mit-krb5)
             ("zlib" ,zlib)))
   (propagated-inputs
    ;; Propagate GnuTLS because libgnutls.la reads `-lnettle', and Nettle is a
    ;; propagated input of GnuTLS.
    `(("gnutls" ,gnutls)))
   (synopsis "Simple Authentication and Security Layer library")
   (description
    "GNU SASL is an implementation of the Simple Authentication and
Security Layer framework.  On network servers such as IMAP or SMTP servers,
SASL is used to handle client/server authentication.  This package contains
both a library and a command-line tool to access the library.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/gsasl/")))
