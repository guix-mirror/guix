;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages which)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages qt))

(define-public gpsbabel
  (package
    (name "gpsbabel")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              ;; XXX: Downloads from gpsbabel.org are hidden behind a POST, so
              ;; get it from elsewhere.
              (uri (string-append
                    "mirror://debian/pool/main/g/gpsbabel/gpsbabel_"
                    version ".orig.tar.gz"))
              (sha256
               (base32
                "1pd01kra9l5ihy1by87qia0mpbpcif7g5yg7r9z2bnw7711jm3yb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--with-zlib=system")
       #:phases (alist-cons-before
                 'configure 'pre-configure
                 (lambda _
                   (chdir "gpsbabel"))
                 ;; TODO: "make doc" requires Docbook & co.
                 %standard-phases)))
    (inputs
     `(("expat" ,expat)
       ("zlib" ,zlib)
       ("qt4" ,qt-4)))
    (native-inputs
     `(("which" ,which)
       ("libxml2" ,libxml2)))              ;'xmllint' needed for the KML tests
    (home-page "http://www.gpsbabel.org/")
    (synopsis "Convert and exchange data with GPS and map programs")
    (description
     "GPSBabel converts waypoints, tracks, and routes between hundreds of
popular GPS receivers and mapping programs.  It contains extensive data
manipulation abilities making it a convenient for server-side processing or as
the back-end for other tools.  It does not convert, transfer, send, or
manipulate maps.")
    (license license:gpl2+)))
