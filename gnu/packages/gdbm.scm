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

(define-module (gnu packages gdbm)
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
    (arguments `(#:configure-flags '("--enable-libgdbm-compat")))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/gdbm/")
    (synopsis
     "Hash library of database functions compatible with traditional dbm")
    (description
     "GDBM is a library for manipulating hashed databases.  It is used to
store key/value pairs in a file in a manner similar to the Unix dbm library
and provides interfaces to the traditional file format.")
    (license gpl3+)))
