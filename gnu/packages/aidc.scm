;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darringon <jmd@gnu.org>
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

(define-module (gnu packages aidc)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))


(define-public barcode
  (package
    (name "barcode")
    (version "0.99")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/barcode/barcode-"
                          version ".tar.xz"))
              (sha256 (base32
                       "1indapql5fjz0bysyc88cmc54y8phqrbi7c76p71fgjp45jcyzp8"))))
    (build-system gnu-build-system)
    (synopsis "Convert text strings to printed bars in various standards")
    (description "GNU Barcode is a flexible tool to produce printed barcodes
from text strings.  It supports a variety of encoding standards and sizing
measurements.  Barcodes can be output in PostScript or Encapsulated PostScript
formats.")
    (license license:gpl3+)
    (home-page "http://www.gnu.org/software/barcode/")))
