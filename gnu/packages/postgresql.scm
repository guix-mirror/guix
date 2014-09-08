;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

(define-module (gnu packages postgresql)
  #:use-module ((guix licenses) #:select (x11-style))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages readline))

(define-public postgresql
  (package
   (name "postgresql")
   (version "9.3.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.postgresql.org/pub/source/v"
                                version "/postgresql-" version ".tar.gz"))
            (sha256
             (base32
              "08kga00izykgvnx7hn995wc4zjqslspapaa8z63045p1ya14mr4g"))))
   (build-system gnu-build-system)
   (inputs
    `(("readline" ,readline)
      ("zlib" ,zlib)))
   (home-page "http://www.postgresql.org/")
   (synopsis "Powerful object-relational database system")
   (description
    "PostgreSQL is a powerful object-relational database system.  It is fully
ACID compliant, has full support for foreign keys, joins, views, triggers, and
stored procedures (in multiple languages).  It includes most SQL:2008 data
types, including INTEGER, NUMERIC, BOOLEAN, CHAR, VARCHAR, DATE, INTERVAL, and
TIMESTAMP.  It also supports storage of binary large objects, including
pictures, sounds, or video.")
   (license (x11-style "file://COPYRIGHT"))))
