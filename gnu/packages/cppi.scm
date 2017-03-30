;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages cppi)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public cppi
  (package
    (name "cppi")
    (version "1.18")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/cppi/cppi-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1jk42cjaggk71rimjnx3qpmb6hivps0917vl3z7wbxk3i2whb98j"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/cppi/")
    (synopsis "Indent C preprocessor directives to reflect nesting and more")
    (description
     "GNU Cppi processes C source code files to properly indent the
preprocessor directives to reflect their nesting.  It also performs other
standardizations, such as correcting the number of spaces between directives
and the text following them.")
    (license gpl3+)))
