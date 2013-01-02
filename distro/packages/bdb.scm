;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Andreas Enge <andreas@enge.fr>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages bdb)
  #:use-module (distro)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public bdb
  (package
   (name "bdb")
   (version "5.3.21")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://download.oracle.com/berkeley-db/db-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1f2g2612lf8djbwbwhxsvmffmf9d7693kh2l20195pqp0f9jmnfx"))))
   (build-system gnu-build-system)
   (arguments
    (lambda (system)
      `(#:tests? #f ; no check target available
        #:phases
        (alist-replace
         'configure
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (zero?
              (system* "./dist/configure"
                       (string-append "--prefix=" out)))))
         %standard-phases))))
   (synopsis "db, the Berkeley database")
   (description
    "Berkeley DB is an embeddable database allowing developers the choice of
SQL, Key/Value, XML/XQuery or Java Object storage for their data model.")
   (license (bsd-style "file://LICENSE"
                       "See LICENSE in the distribution."))
   (home-page "http://www.oracle.com/us/products/database/berkeley-db/overview/index.html")))
