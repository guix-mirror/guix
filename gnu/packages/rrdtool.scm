;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages rrdtool)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages python)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml))

(define-public rrdtool
  (package
    (name "rrdtool")
    (version "1.4.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://oss.oetiker.ch/rrdtool/pub/rrdtool-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1mpki7pv5ql73h5al04dps6dky0nqc3mmb8ac21hd2s8mbsvk5fy"))))
    (build-system gnu-build-system)
    (inputs `(("cairo" ,cairo)
              ("glib" ,glib)
              ("gtk" ,gtk+-2)
              ("pango" ,pango)
              ("freetype" ,freetype)
              ("libxml2" ,libxml2)
              ("python" ,python-2)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("groff" ,groff)))
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'pre-configure
                 (lambda _
                   (substitute* "libtool"
                     (("/bin/sed") (which "sed")))
                   (substitute* "src/Makefile.in"
                     (("^rrdcached_LDADD = librrd_th.la")
                      "rrdcached_LDADD = librrd_th.la -lglib-2.0")))
                 %standard-phases)))
    (home-page "http://oss.oetiker.ch/rrdtool/")
    (synopsis "Time-series data storage and display system")
    (description
     "The Round Robin Database Tool (RRDtool) is a system to store and display
time-series data (e.g. network bandwidth, machine-room temperature, server
load average).  It stores the data in Round Robin Databases (RRDs), a very
compact way that will not expand over time.  RRDtool processes the extracted
data to enforce a certain data density, allowing for useful graphical
representation of data values.")
    (license license:gpl2+))) ; with license exception that allows combining
                              ; with many other licenses.
