;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages python)
  #:use-module ((guix licenses) #:select (psfl))
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages openssl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public python
  (package
    (name "python")
    (version "2.7.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.xz"))
      (sha256
       (base32
        "11f9aw855lrmknr6c82gm1ijr3n0smc6idyp94y7774yivjnplv1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; XXX: some tests fail
       #:patches (list (assoc-ref %build-inputs "patch-dbm"))
       #:patch-flags '("-p0")
       #:configure-flags
        (let ((bz2 (assoc-ref %build-inputs "bzip2"))
              (gdbm (assoc-ref %build-inputs "gdbm"))
              (openssl (assoc-ref %build-inputs "openssl"))
              (readline (assoc-ref %build-inputs "readline"))
              (zlib (assoc-ref %build-inputs "zlib")))
         (list (string-append "CPPFLAGS="
                "-I" bz2 "/include "
                "-I" gdbm "/include "
                "-I" openssl "/include "
                "-I" readline "/include "
                "-I" zlib "/include")
               (string-append "LDFLAGS="
                "-L" bz2 "/lib "
                "-L" gdbm "/lib "
                "-L" openssl "/lib "
                "-L" readline "/lib "
                "-L" zlib "/lib")))))
    (inputs
     `(("bzip2" ,bzip2)
       ("gdbm" ,gdbm)
       ("openssl" ,openssl)
       ("readline" ,readline)
       ("zlib" ,zlib)
       ("patch-dbm" ,(search-patch "python-fix-dbm.patch"))))
    (home-page "http://python.org")
    (synopsis
     "Python, a high-level dynamically-typed programming language")
    (description
     "Python is a remarkably powerful dynamic programming language that
is used in a wide variety of application domains.  Some of its key
distinguishing features include: clear, readable syntax; strong
introspection capabilities; intuitive object orientation; natural
expression of procedural code; full modularity, supporting hierarchical
packages; exception-based error handling; and very high level dynamic
data types.")
    (license psfl)))
