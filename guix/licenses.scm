;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012, 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (guix licenses)
  #:use-module (srfi srfi-9)
  #:export (license? license-name license-uri license-comment
            asl2.0
            boost1.0
            bsd-2 bsd-3 bsd-4 bsd-style
            cddl1.0
            cpl1.0
            epl1.0
            expat
            freetype
            gpl1 gpl1+ gpl2 gpl2+ gpl3 gpl3+
            ijg
            ibmpl1.0
            lgpl2.0 lgpl2.0+ lgpl2.1 lgpl2.1+ lgpl3 lgpl3+
            mpl2.0
            openldap2.8 openssl
            psfl public-domain
            vim
            x11
            zlib))

(define-record-type <license>
  (license name uri comment)
  license?
  (name    license-name)
  (uri     license-uri)
  (comment license-comment))

;;; Commentary:
;;;
;;; Available licenses.
;;;
;;; This list is based on these links:
;;; https://github.com/NixOS/nixpkgs/blob/master/pkgs/lib/licenses.nix
;;; https://www.gnu.org/licenses/license-list
;;;
;;; Code:

(define asl2.0
  (license "ASL 2.0"
           "http://directory.fsf.org/wiki/License:Apache2.0"
           "https://www.gnu.org/licenses/license-list#apache2"))

(define boost1.0
  (license "Boost 1.0"
           "http://directory.fsf.org/wiki/License:Boost1.0"
           "https://www.gnu.org/licenses/license-list#boost"))

(define bsd-2
  (license "FreeBSD"
           "http://directory.fsf.org/wiki/License:FreeBSD"
           "https://www.gnu.org/licenses/license-list#FreeBSD"))

(define bsd-3
  (license "Modified BSD"
           "http://directory.fsf.org/wiki/License:BSD_3Clause"
           "https://www.gnu.org/licenses/license-list#ModifiedBSD"))

(define bsd-4
  (license "Original BSD"
           "http://directory.fsf.org/wiki/License:BSD_4Clause"
           "https://www.gnu.org/licenses/license-list#OriginalBSD"))

(define* (bsd-style uri #:optional (comment ""))
  "Return a BSD-style license, whose full text can be found at URI,
which may be a file:// URI pointing the package's tree."
  (license "BSD-style"
           uri
           (string-append
            "This is a BSD-style, non-copyleft free software license.  "
            "Check the URI for details.  "
            comment)))

(define cddl1.0
  (license "CDDL 1.0"
           "http://directory.fsf.org/wiki/License:CDDLv1.0"
           "https://www.gnu.org/licenses/license-list#CDDL"))

(define cpl1.0
  (license "CPL 1.0"
           "http://directory.fsf.org/wiki/License:CPLv1.0"
           "https://www.gnu.org/licenses/license-list#CommonPublicLicense10"))

(define epl1.0
  (license "EPL 1.0"
           "http://directory.fsf.org/wiki/License:EPLv1.0"
           "https://www.gnu.org/licenses/license-list#EPL"))

(define expat
  (license "Expat"
           "http://directory.fsf.org/wiki/License:Expat"
           "https://www.gnu.org/licenses/license-list.html#Expat"))

(define freetype
  (license "Freetype"
           "http://directory.fsf.org/wiki/License:Freetype"
           "https://www.gnu.org/licenses/license-list.html#freetype"))

(define gpl1
  (license "GPL 1"
           "https://www.gnu.org/licenses/old-licenses/gpl-1.0.html"
           #f))

(define gpl1+
  (license "GPL 1+"
           "https://www.gnu.org/licenses/old-licenses/gpl-1.0.html"
           #f))

(define gpl2
  (license "GPL 2"
           "https://www.gnu.org/licenses/old-licenses/gpl-2.0.html"
           "https://www.gnu.org/licenses/license-list#GPLv2"))

(define gpl2+
  (license "GPL 2+"
           "https://www.gnu.org/licenses/old-licenses/gpl-2.0.html"
           "https://www.gnu.org/licenses/license-list#GPLv2"))

(define gpl3
  (license "GPL 3"
           "https://www.gnu.org/licenses/gpl.html"
           "https://www.gnu.org/licenses/license-list#GNUGPLv3"))

(define gpl3+
  (license "GPL 3+"
           "https://www.gnu.org/licenses/gpl.html"
           "https://www.gnu.org/licenses/license-list#GNUGPLv3"))

(define ijg
  (license "IJG"
           "http://directory.fsf.org/wiki/License:JPEG"
           "https://www.gnu.org/licenses/license-list#ijg"))

(define ibmpl1.0
  (license "IBMPL 1.0"
           "http://directory.fsf.org/wiki/License:IBMPLv1.0"
           "https://www.gnu.org/licenses/license-list#IBMPL"))

(define lgpl2.0
  (license "LGPL 2.0"
           "https://www.gnu.org/licenses/old-licenses/lgpl-2.0.html"
           "https://www.gnu.org/licenses/why-not-lgpl.html"))

(define lgpl2.0+
  (license "LGPL 2.0+"
           "https://www.gnu.org/licenses/old-licenses/lgpl-2.0.html"
           "https://www.gnu.org/licenses/why-not-lgpl.html"))

(define lgpl2.1
  (license "LGPL 2.1"
           "https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html"
           "https://www.gnu.org/licenses/license-list#LGPLv2.1"))

(define lgpl2.1+
  (license "LGPL 2.1+"
           "https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html"
           "https://www.gnu.org/licenses/license-list#LGPLv2.1"))

(define lgpl3
  (license "LGPL 3"
           "https://www.gnu.org/licenses/lgpl.html"
           "https://www.gnu.org/licenses/license-list#LGPLv3"))

(define lgpl3+
  (license "LGPL 3+"
           "https://www.gnu.org/licenses/lgpl.html"
           "https://www.gnu.org/licenses/license-list#LGPLv3"))

(define mpl2.0
  (license "MPL 2.0"
           "http://directory.fsf.org/wiki/License:MPLv2.0"
           "https://www.gnu.org/licenses/license-list#MPL-2.0"))

(define openssl
  (license "OpenSSL"
           "http://directory.fsf.org/wiki/License:OpenSSL"
           "https://www.gnu.org/licenses/license-list#OpenSSL"))

(define openldap2.8
  (license "OpenLDAPv2.8"
           "http://directory.fsf.org/wiki/License:OpenLDAPv2.8"
           "https://www.gnu.org/licenses/license-list#newOpenLDAP"))
              ;; lists OpenLDAPv2.7, which is virtually identical

(define psfl
  (license "Python Software Foundation License"
           "http://docs.python.org/license.html"
           #f))

(define public-domain
  (license "Public Domain"
           "http://directory.fsf.org/wiki/License:PublicDomain"
           "https://www.gnu.org/licenses/license-list#PublicDomain"))

(define vim
  (license "Vim"
           "http://directory.fsf.org/wiki/License:Vim7.2"
           "http://www.gnu.org/licenses/license-list.html#Vim"))

(define x11
  (license "X11"
           "http://directory.fsf.org/wiki/License:X11"
           "https://www.gnu.org/licenses/license-list#X11License"))

(define zlib
  (license "Zlib"
           "http://www.gzip.org/zlib/zlib_license.html"
           "https://www.gnu.org/licenses/license-list#ZLib"))

;;; licenses.scm ends here
