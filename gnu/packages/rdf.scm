;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
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
  #:use-module ((guix licenses) #:select (lgpl2.0+ lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml))

(define-public raptor2
  (package
    (name "raptor2")
    (version "2.0.11")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.librdf.org/source/" name
                                 "-" version ".tar.gz"))
             (sha256
              (base32
               "1z3i4fs32wcq5y9l7gdn7262h6r0ppdpvx4gw6jgsym8z083w0xf"))))
    (build-system gnu-build-system)
    (inputs
     `(("curl" ,curl)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("zlib" ,zlib)))
    (arguments
     `(#:parallel-tests? #f))
    (home-page "http://librdf.org/raptor/")
    (synopsis "RDF syntax library")
    (description "Raptor is a C library providing a set of parsers and
serialisers that generate Resource Description Framework (RDF) triples
by parsing syntaxes or serialise the triples into a syntax.  The supported
parsing syntaxes are RDF/XML, N-Quads, N-Triples 1.0 and 1.1, TRiG,
Turtle 2008 and 2013, RDFa 1.0 and 1.1, RSS tag soup including all versions
of RSS, Atom 1.0 and 0.3, GRDDL and microformats for HTML, XHTML and
XML.  The serialising syntaxes are RDF/XML (regular, abbreviated, XMP),
Turtle 2013, N-Quads, N-Triples 1.1, Atom 1.0, RSS 1.0, GraphViz DOT,
HTML and JSON.")
    (license lgpl2.1+))) ; or any choice of gpl2+ or asl2.0

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
     `(("qt" ,qt-4)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://soprano.sourceforge.net/")
    (synopsis "RDF data library for Qt")
    (description "Soprano (formerly known as QRDF) is a library which
provides a highly usable object-oriented C++/Qt4 framework for RDF data.  It
uses different RDF storage solutions as backends through a simple plugin
system.")
    (license lgpl2.0+)))
