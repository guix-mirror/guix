;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages groff)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo))

(define-public groff
  (package
   (name "groff")
   (version "1.22.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/groff/groff-" version
                                ".tar.gz"))
            (sha256 (base32
                     "0xi07nhj5vdgax37rj25mwxzdmsz1ifx50hjgc6hqbkpqkd6821q"))))
   (build-system gnu-build-system)
   (inputs `(("bison" ,bison)
             ("ghostscript" ,ghostscript)
             ("netpbm" ,netpbm)
             ("perl" ,perl)
             ("psutils" ,psutils)
             ("texinfo" ,texinfo)))
   (synopsis "Typesetting from plain text mixed with formatting commands")
   (description
    "GNU Troff (Groff) is a software typesetting package which reads plain
text mixed with formatting commands and produces formatted output.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/groff/")))
