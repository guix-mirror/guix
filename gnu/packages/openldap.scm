;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages openldap)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:select (openldap2.8 lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public openldap
  (package
   (name "openldap")
   (version "2.4.45")
   (source (origin
            (method url-fetch)

            ;; See <http://www.openldap.org/software/download/> for a list of
            ;; mirrors.
            (uri (list (string-append
                        "ftp://mirror.switch.ch/mirror/OpenLDAP/"
                        "openldap-release/openldap-" version ".tgz")
                       (string-append
                        "https://www.openldap.org/software/download/OpenLDAP/"
                        "openldap-release/openldap-" version ".tgz")
                       (string-append
                        "ftp://ftp.dti.ad.jp/pub/net/OpenLDAP/"
                        "openldap-release/openldap-" version ".tgz")))
            (sha256
             (base32
              "091qvwk5dkcpp17ziabcnh3rg3m7qwzw2pihfcd1d5fdxgywzmnd"))))
   (build-system gnu-build-system)
   (inputs `(("bdb" ,bdb-5.3)
             ("cyrus-sasl" ,cyrus-sasl)
             ("gnutls" ,gnutls)
             ("groff" ,groff)
             ("icu4c" ,icu4c)
             ("libgcrypt" ,libgcrypt)
             ("zlib" ,zlib)))
   (native-inputs `(("libtool" ,libtool)))
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'configure 'provide-libtool
          (lambda _ (copy-file (which "libtool") "libtool")
            #t))
        (add-after 'install 'patch-sasl-path
          ;; Give -L arguments for cyrus-sasl to avoid propagation.
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (sasl (assoc-ref inputs "cyrus-sasl")))
              (substitute* (map (lambda (f) (string-append out "/" f))
                                '("lib/libldap.la" "lib/libldap_r.la"))
                (("-lsasl2" lib)
                 (string-append "-L" sasl "/lib " lib)))
              #t))))))
   (synopsis "Implementation of the Lightweight Directory Access Protocol")
   (description
    "OpenLDAP is a free implementation of the Lightweight Directory Access Protocol.")
   (license openldap2.8)
   (home-page "https://www.openldap.org/")))

(define-public nss-pam-ldapd
  (package
    (name "nss-pam-ldapd")
    (version "0.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://arthurdejong.org/nss-pam-ldapd/"
                                  "nss-pam-ldapd-" version ".tar.gz"))
              (sha256
               (base32
                "1lj7qkjlg3bshwdc5x5r1ny37rly4wgm1c8b6w6b5f4wa11nmji0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-pam-seclib-dir="
                            (assoc-ref %outputs "out") "/lib/security/")
             ;; nslcd cannot be convinced to look at run-time for its
             ;; configuration file at a location that differs from the
             ;; configured location.
             "--with-ldap-conf-file=/etc/nslcd.conf")
       #:phases
       (modify-phases %standard-phases
         ;; This is necessary because we tell nslcd with configure flags that
         ;; it should look for its configuration file at /etc/nslcd.conf.  The
         ;; build system tries to install a default configuration to that very
         ;; location.
         (add-after 'unpack 'override-nslcd.conf-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("\\$\\(DESTDIR\\)\\$\\(NSLCD_CONF_PATH\\)")
                (string-append (assoc-ref outputs "out")
                               "/etc/nslcd.conf.example")))
             #t)))))
    (inputs
     `(("linux-pam" ,linux-pam)
       ("openldap" ,openldap)
       ("mit-krb5" ,mit-krb5)
       ("python" ,python-2)))
    (home-page "https://arthurdejong.org/nss-pam-ldapd")
    (synopsis "NSS and PAM modules for LDAP")
    (description "nss-pam-ldapd provides a @dfn{Name Service Switch} (NSS)
module that allows your LDAP server to provide user account, group, host name,
alias, netgroup, and basically any other information that you would normally
get from @file{/etc} flat files or NIS.  It also provides a @dfn{Pluggable
Authentication Module} (PAM) to do identity and authentication management with
an LDAP server.")
    (license lgpl2.1+)))
