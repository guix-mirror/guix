;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
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

(define-module (gnu packages authentication)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public oath-toolkit
  (package
    (name "oath-toolkit")
    (version "2.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.savannah.nongnu.org/releases/"
                           name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1aa620k05lsw3l3slkp2mzma40q3p9wginspn9zk8digiz7dzv9n"))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO ‘--enable-pskc’ causes xmlsec-related test suite failures.
     `(#:configure-flags
       (list "--enable-pam"
             "--enable-pskc"
             "--with-xmlsec-crypto-engine=openssl")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'delete-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each delete-file (find-files lib "\\.a$"))
               #t))))))
    (native-inputs
     (list pkg-config
           ;; XXX: Perhaps this should be propagated from xmlsec.
           libltdl))
    (inputs
     (list linux-pam openssl xmlsec-openssl))
    (home-page "https://www.nongnu.org/oath-toolkit/")
    (synopsis "One-time password (OTP) components")
    (description
     "The @dfn{OATH} (Open AuTHentication) Toolkit provides various components
for building one-time password (@dfn{OTP}) authentication systems:

@itemize
@item @command{oathtool}, a command-line tool for generating & validating OTPs.
@item @code{liboath}, a C library for OATH handling.
@item @command{pskctool}, a command-line tool for manipulating secret key
files in the Portable Symmetric Key Container (@dfn{PSKC}) format
described in RFC6030.
@item @code{libpskc}, a shared and static C library for PSKC handling.
@item @code{pam_oath}, a PAM module for pluggable login authentication.
@end itemize

Supported technologies include the event-based @acronym{HOTP, Hash-based Message
Authentication Code One-Time Password} algorithm (RFC4226), the time-based
@acronym{TOTP, Time-based One-Time Password} algorithm (RFC6238), and
@acronym{PSKC, Portable Symmetric Key Container} (RFC6030) to manage secret key
data.")
    (license (list license:lgpl2.1+     ; the libraries (liboath/ & libpskc/)
                   license:gpl3+))))    ; the tools (everything else)

(define-public oauth2l
  (package
    (name "oauth2l")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/oauth2l")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a9x0b31ybyjg0k7923xw6zr6crm0kigcn8g6hyr228nbvw35r8w"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/oauth2l"))
    (home-page "https://github.com/google/oauth2l")
    (synopsis "Simple CLI for interacting with Google API authentication")
    (description
     "@code{oauth2l} (pronounced ``oauth tool'') is a simple command-line tool
for working with @url{https://developers.google.com/identity/protocols/OAuth2,
Google OAuth 2.0} written in Go.  Its primary use is to fetch and print OAuth
2.0 access tokens, which can be used with other command-line tools and
scripts.")
    (license license:asl2.0)))

(define-public yubico-pam
  (package
    (name "yubico-pam")
    (version "2.27")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Yubico/yubico-pam")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hb773zlf11xz4bwmsqv2mq5d4aq2g0crdr5cp9xwc4ivi5gd4kg"))))
    (build-system gnu-build-system)
    (arguments
     ;; The pam_test fails because ykclient fails to build a Curl handle.
     '(#:make-flags '("TESTS=util_test")))
    (inputs
     (list linux-pam libyubikey ykclient yubikey-personalization))
    (native-inputs
     (list autoconf automake libtool asciidoc pkg-config))
    (home-page "https://developers.yubico.com/yubico-pam")
    (synopsis "Yubico pluggable authentication module")
    (description "The Yubico PAM module provides an easy way to integrate the
YubiKey into your existing user authentication infrastructure.")
    (license license:bsd-2)))

(define-public pamtester
  (package
    (name "pamtester")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/pamtester/pamtester/"
             version "/pamtester-" version ".tar.gz"))
       (sha256
        (base32 "1mdj1wj0adcnx354fs17928yn2xfr1hj5mfraq282dagi873sqw3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list linux-pam))
    (home-page "http://pamtester.sourceforge.net/")
    (synopsis "Utility for testing pluggable authentication modules (PAM) facility")
    (description
     "Pamtester is a tiny utility program to test the pluggable authentication
modules (PAM) facility, specifically designed to help PAM module authors to
intensively test their own modules.")
    (license license:bsd-3)))
