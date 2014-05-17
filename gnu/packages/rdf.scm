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
  #:use-module ((guix licenses) #:select (lgpl2.0+ lgpl2.1 lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bdb)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages doxygen)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
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

(define-public clucene
  (package
    (name "clucene")
    (version "2.3.3.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/clucene/"
                                 "clucene-core-unstable/2.3/clucene-core-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1arffdwivig88kkx685pldr784njm0249k0rb1f1plwavlrw9zfx"))
             (patches (list (search-patch "clucene-pkgconfig.patch")))))
    (build-system cmake-build-system)
    (inputs
     `(("boost" ,boost) ; could also use bundled copy
       ("zlib" ,zlib)))
    (arguments
     `(#:test-target "cl_test"
       #:tests? #f)) ; Tests do not compile, as TestIndexSearcher.cpp uses
                     ; undeclared usleep. After fixing this, one needs to run
                     ; "make test" in addition to "make cl_test", then
                     ; SimpleTest fails.
                     ; Notice that the library appears to be unmaintained
                     ; with no reaction to bug reports.
    (home-page "http://clucene.sourceforge.net/")
    (synopsis "C text indexing and searching library")
    (description "CLucene is a high-performance, scalable, cross platform,
full-featured indexing and searching API.  It is a port of the very popular
Java Lucene text search engine API to C++.")
    (license lgpl2.1)))

(define-public rasqal
  (package
    (name "rasqal")
    (version "0.9.32")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.librdf.org/source/" name
                                 "-" version ".tar.gz"))
             (sha256
              (base32
               "13rfprkk7d74065c7bafyshajwa6lshj7m9l741zlz9viqhh7fpf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("perl-xml-dom" ,perl-xml-dom) ; for the tests
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("libxml2" ,libxml2)
       ("mpfr" ,mpfr)
       ("pcre" ,pcre)
       ("util-linux" ,util-linux)))
    (propagated-inputs
     `(("raptor2" ,raptor2))) ; stipulated by rasqal.pc
    (arguments
     `(#:parallel-tests? #f
       ; test failure reported upstream, see
       ; http://bugs.librdf.org/mantis/view.php?id=571
       #:tests? #f))
    (home-page "http://librdf.org/rasqal/")
    (synopsis "RDF query library")
    (description "Rasqal is a C library that handles Resource Description
Framework (RDF) query language syntaxes, query construction and execution
of queries returning results as bindings, boolean, RDF graphs/triples or
syntaxes.  The supported query languages are SPARQL Query 1.0,
SPARQL Query 1.1, SPARQL Update 1.1 (no executing) and the Experimental
SPARQL extensions (LAQRS).  Rasqal can write binding query results in the
SPARQL XML, SPARQL JSON, CSV, TSV, HTML, ASCII tables, RDF/XML and
Turtle/N3 and read them in SPARQL XML, RDF/XML and Turtle/N3.")
    (license lgpl2.1+))) ; or any choice of gpl2+ or asl2.0

(define-public redland
  (package
    (name "redland")
    (version "1.0.17")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.librdf.org/source/" name
                                 "-" version ".tar.gz"))
             (sha256
              (base32
               "109n0kp39p966dpiasad2bb7q66rwbcb9avjvimw28chnpvlf66y"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl) ; needed for installation
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bdb" ,bdb)
       ("rasqal" ,rasqal)))
    (home-page "http://librdf.org/")
    (synopsis "RDF library")
    (description "The Redland RDF Library (librdf) provides the RDF API
and triple stores.")
    (license lgpl2.1+))) ; or any choice of gpl2+ or asl2.0

(define-public soprano
  (package
    (name "soprano")
    (version "2.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/soprano/Soprano/"
                                version "/"
                                "soprano-" version ".tar.bz2"))
             (sha256
              (base32
               "1rg0x7yg0a1cbnxz7kqk52580wla8jbnj4d4r3j7l7g7ajyny1k4"))
             (patches (list (search-patch "soprano-find-clucene.patch")))))
    (build-system cmake-build-system)
    ;; FIXME: Add optional dependencies: Redland, odbci.
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("clucene" ,clucene)
       ("qt" ,qt-4)
       ("raptor2" ,raptor2)))
    (home-page "http://soprano.sourceforge.net/")
    (synopsis "RDF data library for Qt")
    (description "Soprano (formerly known as QRDF) is a library which
provides a highly usable object-oriented C++/Qt4 framework for RDF data.  It
uses different RDF storage solutions as backends through a simple plugin
system.")
    (license lgpl2.0+)))
