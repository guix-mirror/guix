;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages gdbm)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public gdbm
  (package
    (name "gdbm")
    (version "1.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gdbm/gdbm-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0h9lfzdjc2yl849y0byg51h6xfjg0y7vg9jnsw3gpfwlbd617y13"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/gdbm/")
    (synopsis "GNU dbm key/value database library")
    (description
     "GNU dbm (or GDBM, for short) is a library of database functions
that use extensible hashing and work similar to the standard UNIX dbm.
These routines are provided to a programmer needing to create and
manipulate a hashed database.

The basic use of GDBM is to store key/data pairs in a data file.  Each
key must be unique and each key is paired with only one data item.

The library provides primitives for storing key/data pairs, searching
and retrieving the data by its key and deleting a key along with its
data.  It also support sequential iteration over all key/data pairs in a
database.

For compatibility with programs using old UNIX dbm function, the package
also provides traditional dbm and ndbm interfaces.")
    (license gpl3+)))
