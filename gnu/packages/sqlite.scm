;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages sqlite)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public sqlite
  (package
   (name "sqlite")
   (version "3.8.4.2")
   (source (origin
            (method url-fetch)
            ;; TODO: Download from sqlite.org once this bug :
            ;; http://lists.gnu.org/archive/html/bug-guile/2013-01/msg00027.html
            ;; has been fixed.
            (uri (let ((numeric-version
                        (match (string-split version #\.)
                          ((first-digit other-digits ...)
                           (string-append first-digit
                                          (string-pad-right
                                           (string-concatenate
                                            (map (cut string-pad <> 2 #\0)
                                                 other-digits))
                                           6 #\0))))))
                   (string-append
                    "mirror://sourceforge/sqlite.mirror/SQLite%20" version
                    "/sqlite-autoconf-" numeric-version ".tar.gz")))
            (sha256
             (base32
              "19gicv5vdi5c0p8shr1bmihldj409aqz3r4wr7d3pwb6xf1xv4p4"))
            (patches
             (list (search-patch "sqlite-large-page-size-fix.patch")))))
   (build-system gnu-build-system)
   (home-page "http://www.sqlite.org/")
   (synopsis "The SQLite database management system")
   (description
    "SQLite is a software library that implements a self-contained, serverless,
zero-configuration, transactional SQL database engine. SQLite is the most
widely deployed SQL database engine in the world. The source code for SQLite is
in the public domain.")
   (license public-domain)))
