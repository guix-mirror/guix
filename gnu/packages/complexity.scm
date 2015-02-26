;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages complexity)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages autogen))

(define-public complexity
  (package
    (name "complexity")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/complexity/complexity-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0pmlhlj1chl4caaqffvn1cy9z8gwmjbx97syi7pdfa0vqygkql6d"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("texinfo" ,texinfo)
       ("autogen" ,autogen)))
    (home-page "http://www.gnu.org/software/complexity/")
    (synopsis "Analyze complexity of C functions")
    (description
     "GNU complexity provides tools for finding procedures that are
convoluted, overly long or otherwise difficult to understand.  This
may help in learning or reviewing unfamiliar code or perhaps
highlighting your own code that seemed comprehensible when you wrote it.")
    (license gpl3+)))
