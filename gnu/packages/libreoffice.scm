;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
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

(define-module (gnu packages libreoffice)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (mpl2.0))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public ixion
  (package
    (name "ixion")
    (version "0.9.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://kohei.us/files/ixion/src/libixion-"
                          version ".tar.xz"))
      (sha256 (base32
               "18g3nk29ljiqbyi0ml49j2x3f3xrqckdm9i66sw5fxnj7hb5rqvp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
     (inputs
      `(("mdds" ,mdds)
        ("python" ,python-2))) ; looks for python.pc, not python3.pc
    (home-page "https://gitlab.com/ixion/ixion")
    (synopsis "General purpose formula parser and interpreter")
    (description "Ixion is a library for calculating the results of formula
expressions stored in multiple named targets, or \"cells\".  The cells can
be referenced from each other, and the library takes care of resolving
their dependencies automatically upon calculation.")
    (license mpl2.0)))

(define-public orcus
  (package
    (name "orcus")
    (version "0.7.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://kohei.us/files/" name "/src/lib"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "0hva4qalg3dk6n1118ncr5fy8cqnj2f7fwldw7aa04124rj6p104"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
     (inputs
      `(("ixion" ,ixion)
        ("mdds" ,mdds)
        ("zlib" ,zlib)))
    (home-page "https://gitlab.com/orcus/orcus")
    (synopsis "File import filter library for spreadsheet documents")
    (description "Orcus is a library that provides a collection of standalone
file processing filters.  It is currently focused on providing filters for
spreadsheet documents.  The library includes import filters for
Microsoft Excel 2007 XML, Microsoft Excel 2003 XML, Open Document Spreadsheet,
Plain Text, Gnumeric XML, Generic XML.  It also includes low-level parsers for
CSV, CSS and XML.")
    (license mpl2.0)))
