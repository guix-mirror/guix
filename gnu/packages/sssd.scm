;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages augeas)
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
    (version "1.16.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://releases.pagure.org/SSSD/sssd/"
                                  "sssd-" version ".tar.gz"))
              (patches (search-patches "sssd-curl-compat.patch"))
              (sha256
               (base32
                "032ppk57qs1lnvz7pb7lw9ldwm9i1yagh9fzgqgn6na3bg61ynzy"))))
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
       (list "--disable-cifs-idmap-plugin"
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
             ;; XXX: These tests fail with recent versions of ldb.  See
             ;; <https://pagure.io/SSSD/sssd/issue/3563>.
             (substitute* "Makefile.in"
               (("sysdb-tests\\$\\(EXEEXT\\)") ""))
             #t)))))
    (inputs
     `(("augeas" ,augeas)
       ("bind" ,isc-bind "utils")
       ("c-ares" ,c-ares)
       ("curl" ,curl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("dbus" ,dbus)
       ("ding-libs" ,ding-libs)
       ("glib" ,glib)
       ("gnutls" ,gnutls)
       ("http-parser" ,http-parser)
       ("jansson" ,jansson)
       ("ldb" ,ldb)
       ("libselinux" ,libselinux)
       ("libsemanage" ,libsemanage)
       ("libunistring" ,libunistring)
       ("linux-pam" ,linux-pam)
       ("mit-krb5" ,mit-krb5)
       ("nss" ,nss)
       ("openldap" ,openldap)
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("popt" ,popt)
       ("samba" ,samba)
       ("talloc" ,talloc)
       ("tdb" ,tdb)
       ("tevent" ,tevent)))
    (native-inputs
     `(("check" ,check)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml)
       ("libxml2" ,libxml2)             ; for xmllint
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("util-linux" ,util-linux)))     ; for uuid.h, reqired for KCM
    (home-page "https://pagure.io/SSSD/sssd/")
    (synopsis "System security services daemon")
    (description "SSSD is a system daemon.  Its primary function is to provide
access to identity and authentication remote resource through a common
framework that can provide caching and offline support to the system.  It
provides PAM and NSS modules, and in the future will support D-BUS based
interfaces for extended user information.  It also provides a better database
to store local users as well as extended user data.")
    (license license:gpl3+)))
