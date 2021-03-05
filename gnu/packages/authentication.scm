;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public oath-toolkit
  (package
    (name "oath-toolkit")
    (version "2.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.savannah.nongnu.org/releases/"
                           name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0v4lrgip08b8xlivsfn3mwql3nv8hmcpzrn6pi3xp88vqwav6s7x"))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO ‘--enable-pskc’ causes xmlsec-related test suite failures.
     `(#:configure-flags
       (list "--enable-pam")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'delete-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each delete-file (find-files lib "\\.a$"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("linux-pam" ,linux-pam)))       ; for --enable-pam
    (home-page "https://www.nongnu.org/oath-toolkit/")
    (synopsis "One-time password (OTP) components")
    ;; TODO Add the following items after they've been enabled.
    ;; @item @command{pskctool}, a command-line tool for manipulating secret key
    ;; files in the Portable Symmetric Key Container (@dfn{PSKC}) format
    ;; described in RFC6030.
    ;; @item @code{libpskc}, a shared and static C library for PSKC handling.
    (description
     "The @dfn{OATH} (Open AuTHentication) Toolkit provides various components
for building one-time password (@dfn{OTP}) authentication systems:

@itemize
@item @command{oathtool}, a command-line tool for generating & validating OTPs.
@item @code{liboath}, a C library for OATH handling.
@item @code{pam_oath}, a PAM module for pluggable login authentication.
@end itemize

Supported technologies include the event-based @dfn{HOTP} algorithm (RFC4226)
and the time-based @dfn{TOTP} algorithm (RFC6238).")
    (license (list license:lgpl2.1+     ; the libraries (liboath/ & libpskc/)
                   license:gpl3+))))    ; the tools (everything else)

(define-public yubico-pam
  (let ((commit "b5bd00db81e0e0e0ecced65c684080bb56ddc35b")
        (revision "0"))
    (package
      (name "yubico-pam")
      (version (git-version "2.26" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Yubico/yubico-pam")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10dq8dqi3jldllj6p8r9hldx9sank9n82c44w8akxrs1vli6nj3m"))))
      (build-system gnu-build-system)
      (arguments
       ;; The pam_test fails because ykclient fails to build a Curl handle.
       '(#:make-flags '("TESTS=util_test")))
      (inputs
       `(("linux-pam" ,linux-pam)
         ("libyubikey" ,libyubikey)
         ("ykclient" ,ykclient)
         ("yubikey-personalization" ,yubikey-personalization)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("asciidoc" ,asciidoc)
         ("pkg-config" ,pkg-config)))
      (home-page "https://developers.yubico.com/yubico-pam")
      (synopsis "Yubico pluggable authentication module")
      (description "The Yubico PAM module provides an easy way to integrate the
YubiKey into your existing user authentication infrastructure.")
      (license license:bsd-2))))

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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("linux-pam" ,linux-pam)))
    (home-page "http://pamtester.sourceforge.net/")
    (synopsis "Utility for testing pluggable authentication modules (PAM) facility")
    (description
     "Pamtester is a tiny utility program to test the pluggable authentication
modules (PAM) facility, specifically designed to help PAM module authors to
intensively test their own modules.")
    (license license:bsd-3)))
