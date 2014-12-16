;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
   (version "1.22.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/groff/groff-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1998v2kcs288d3y7kfxpvl369nqi06zbbvjzafyvyl3pr7bajj1s"))))
   (build-system gnu-build-system)
   (inputs `(("ghostscript" ,ghostscript)
             ("netpbm" ,netpbm)))
   (native-inputs `(("bison" ,bison)
                    ("perl" ,perl)
                    ("psutils" ,psutils)
                    ("texinfo" ,texinfo)))
   (arguments '(#:parallel-build? #f))  ; parallel build fails
   (synopsis "Typesetting from plain text mixed with formatting commands")
   (description
    "Groff is a typesetting package that reads plain text and produces
formatted output based on formatting commands contained within the text.  It
is usually the formatter of \"man\" documentation pages.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/groff/")))
