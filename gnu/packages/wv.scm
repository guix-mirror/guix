;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages wv)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public wv
  (package
    (name "wv")
    (version "1.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://abiword.org/downloads/wv/"
                           version "/wv-" version ".tar.gz"))
       (sha256
        (base32 "17f16lkdv1c3amaz2hagiicih59ynpp4786k1m2qa1sw68xhswsc"))))
    (build-system gnu-build-system)
    (inputs
      (list glib libgsf libjpeg-turbo libpng zlib))
    (native-inputs
      (list `(,glib "bin") pkg-config))
    (synopsis "Microsoft Word conversion library and utilities")
    (description
     "wv converts files written by Word 2000, 97, 95, and 6 (known internally as
Word 9, 8, 7, and 6) to HTML or LaTeX.  Word 2 documents can still be converted
to plain text but will lack formatting.

Other programs can use wv as a library to convert Word documents to other
formats.  AbiWord uses it as its Word importer, and KWord uses concepts and
code from wv in theirs.")
    (home-page "http://wvware.sourceforge.net/")
    (license license:gpl2+)))
