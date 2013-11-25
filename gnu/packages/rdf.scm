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

(define-module (gnu packages rdf)
  #:use-module ((guix licenses) #:select (lgpl2.0+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

(define-public soprano
  (package
    (name "soprano")
    (version "2.9.3")
    ;; 2.9.4 requires clucene, see
    ;; http://www.mailinglistarchive.com/html/lfs-book@linuxfromscratch.org/2013-10/msg00285.html
    ;; The stable clucene-0.9.21b fails one of its tests;
    ;; in the unstable clucene-2.3.3.4 the binary cl_test is not found.
    ;; In any case, the library seems to be unmaintained.
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/soprano/Soprano/"
                                version "/"
                                "soprano-" version ".tar.bz2"))
             (sha256
              (base32
               "08gb5d8bgy7vc6qd6r1kkmmc5rli67dlglpjqjlahpnvs26r1cwl"))))
    (build-system cmake-build-system)
    ;; FIXME: Add optional dependencies: Raptor, Redland, odbci, clucene; doxygen
    (inputs
     `(("pkg-config" ,pkg-config)
       ("qt" ,qt-4)))
    (home-page "http://soprano.sourceforge.net/")
    (synopsis "RDF data library for Qt")
    (description "Soprano (formerly known as QRDF) is a library which
provides a highly usable object-oriented C++/Qt4 framework for RDF data.  It
uses different RDF storage solutions as backends through a simple plugin
system.")
    (license lgpl2.0+)))
