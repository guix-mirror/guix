;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2021, 2022 Remco van 't Veer <remco@remworks.net>
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

(define-module (gnu packages sssd)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages augeas)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public adcli
  (package
    (name "adcli")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/realmd/adcli.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mwzd5vakdsssdvs6vljqpp8pw8i97n5lhxvmn9dn9720am7hfv7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; The net tool is used to update the stored machine key for samba.
       (list (string-append "--with-samba-data-tool="
                            (assoc-ref %build-inputs "samba") "/bin/net"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-local-docbook
           ;; Patch Makefile and docs to use local docbook resources.
           (lambda _
             (let* ((docbook-xml (assoc-ref %build-inputs "docbook-xml"))
                    (docbook-xsl (assoc-ref %build-inputs "docbook-xsl"))
                    (xsldir (string-append docbook-xsl "/xml/xsl/docbook-xsl-"
                                           ,(package-version docbook-xsl))))
                    (with-directory-excursion "doc"
                      (substitute*
                          '("Makefile.am" "adcli.xml" "adcli-devel.xml" "adcli-docs.xml")
                        (("http://docbook.sourceforge.net/release/xsl/current(/[^\"]*)" _ path)
                         (string-append xsldir path))
                        (("http://www.oasis-open.org/docbook/xml/4.3/docbookx.dtd")
                         (string-append docbook-xml "/xml/dtd/docbook/docbookx.dtd")))
                      (substitute* "Makefile.am"
                        (("\\$\\(XMLTO\\)" xmlto)
                         (string-append xmlto " --searchpath " xsldir "/html"))))))))))
    (native-inputs
     (list autoconf
           automake
           docbook-xml
           docbook-xsl
           libtool
           libxslt
           util-linux ; For `rev` command used in tests.
           xmlto))
    (inputs
     (list cyrus-sasl mit-krb5 samba openldap))
    (home-page "https://gitlab.freedesktop.org/realmd/adcli/")
    (synopsis "Helper library and tools for Active Directory client operations")
    (description "@command{adcli} is a command‐line tool to join a computer to
an Active Directory domain.  It can also update the machine password and
manage user, group and computer accounts for a domain.")
    (license license:lgpl2.1+)))

(define-public ding-libs
  (package
    (name "ding-libs")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/SSSD/ding-libs/"
                                  "ding-libs-" version ".tar.gz"))
              (sha256
               (base32
                "1h97mx2jdv4caiz4r7y8rxfsq78fx0k4jjnfp7x2s7xqvqks66d3"))))
    (build-system gnu-build-system)
    (home-page "https://pagure.io/SSSD/ding-libs/")
    (synopsis "Libraries for SSSD")
    (description
     "DING-LIBS (DING Is Not Glib) are a set of small, useful libraries that
the @dfn{System Security Services Daemon} (SSSD) uses and makes available to
other projects.  They include: libdhash, an implementation of a dynamic hash
table which will dynamically resize to achieve optimal storage and access time
properties; ini_config, a library for parsing and managing @code{INI} files;
path_utils, a library to manage UNIX paths and subsets of paths; collection, a
generic, hierarchical grouping mechanism for complex data sets; ref_array, a
dynamically-growing, reference-counted array; libbasicobjects, a set of
fundamental object types for C.")
    (license license:lgpl3+)))

;; Note: This package installs modules for ldb and nss.  For the former we
;; need to set LDB_MODULES_PATH.  For the latter LD_PRELOAD or LD_LIBRARY_PATH
;; is needed.
(define-public sssd
  (package
    (name "sssd")
    (version "1.16.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.pagure.org/SSSD/sssd/"
                                  "sssd-" version ".tar.gz"))
              (sha256
               (base32
                "1h6hwibaf3xa2w6qpzjiiywmfj6zkgbz4r2isf3gd0xm6vq7n6if"))
              (patches (search-patches "sssd-fix-samba.patch"
                                       "sssd-system-directories.patch"
                                       "sssd-collision-with-external-nss-symbol.patch"
                                       "sssd-fix-samba-4.15.3.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "DOCBOOK_XSLT="
                            (assoc-ref %build-inputs "docbook-xsl")
                            "/xml/xsl/docbook-xsl-"
                            ,(package-version docbook-xsl)
                            "/manpages/docbook.xsl")
             ;; Remove "--postvalid" option, because that requires access to
             ;; online DTDs.
             "XMLLINT_FLAGS = --catalogs --nonet --noent --xinclude --noout")
       #:configure-flags
       (list "--localstatedir=/var" ;for /var/lib/sss, /var/run/sssd.pid, etc.
             "--sysconfdir=/etc"    ;/etc/sssd

             "--disable-cifs-idmap-plugin"
             "--without-nfsv4-idmapd-plugin"
             "--without-python2-bindings"
             "--without-python3-bindings"
             (string-append "--with-plugin-path="
                            (assoc-ref %outputs "out")
                            "/lib/sssd")
             (string-append "--with-krb5-plugin-path="
                            (assoc-ref %outputs "out")
                            "/lib/krb5/plugins/libkrb5")
             (string-append "--with-cifs-plugin-path="
                            (assoc-ref %outputs "out")
                            "/lib/cifs-utils")
             (string-append "--with-init-dir="
                            (assoc-ref %outputs "out")
                            "/etc/init.d")
             (string-append "--with-ldb-lib-dir="
                            (assoc-ref %outputs "out")
                            "/lib/ldb/modules/ldb")
             (string-append "--with-xml-catalog-path="
                            (assoc-ref %build-inputs "docbook-xml")
                            "/xml/dtd/docbook/catalog.xml"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-test
           (lambda _
             (substitute* "src/tests/responder_socket_access-tests.c"
               (("tcase_add_test\\(tc_utils, resp_str_to_array_test\\);") ""))
             #t))
         (add-after 'unpack 'add-config-in
           (lambda _
             (let ((config.h (open-file "config.h.in" "a")))
               (display (string-append "
/* Missing in commits on original repo, dunno why but won't work without. */
#undef SMB_HAS_NEW_NDR_PULL_STEAL_SWITCH
")
                        config.h)
               (close config.h))))
         (add-before 'configure 'autoconf
           (lambda _
             (invoke "autoconf"))))))
    (inputs
     (list augeas
           `(,isc-bind "utils")
           c-ares
           curl
           cyrus-sasl
           dbus
           ding-libs
           glib
           gnutls
           http-parser
           jansson
           ldb
           libselinux
           libsemanage
           libunistring
           linux-pam
           mit-krb5
           nss
           openldap
           openssl
           pcre
           popt
           samba
           talloc
           tdb
           tevent))
    (native-inputs
     (list autoconf-2.69
           check-0.14
           docbook-xsl
           docbook-xml
           libxml2 ; for xmllint
           libxslt
           pkg-config
           `(,util-linux "lib"))) ;for uuid.h, reqired for KCM
    (home-page "https://pagure.io/SSSD/sssd/")
    (synopsis "System security services daemon")
    (description "SSSD is a system daemon.  Its primary function is to provide
access to identity and authentication remote resource through a common
framework that can provide caching and offline support to the system.  It
provides PAM and NSS modules, and in the future will support D-BUS based
interfaces for extended user information.  It also provides a better database
to store local users as well as extended user data.")
    (license license:gpl3+)))
