;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages pem)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl))

(define-public pem
  (package
    (name "pem")
    (version "0.7.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pem/pem-"
                          version ".tar.gz"))
      (sha256
       (base32
        "03iqcki1lakkck1akdyvljjapgqda3l0rh38id7jhrac9kcxqgg2"))))
    (build-system gnu-build-system)
    (inputs (list perl))
    (home-page "https://www.gnu.org/software/pem/")
    (synopsis "Personal expenses manager")
    (description
     "GNU Pem is a simple tool for tracking personal income and
expenses.  It operates from the command line and it stores its data
in a basic text format in your home directory.  It can easily print
reports of your spending on different expenses via a basic search
feature.")
    (license gpl3+)))
