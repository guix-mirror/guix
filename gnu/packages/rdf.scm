;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module ((guix licenses)
                #:select (non-copyleft asl2.0 isc gpl2 lgpl2.1 lgpl2.1+ lgpl3+))
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public raptor2
  (package
    (name "raptor2")
    (version "2.0.15")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.librdf.org/source/" name
                                 "-" version ".tar.gz"))
             (patches
              (search-patches "raptor2-heap-overflow.patch"))
             (sha256
              (base32
               "1vc02im4mpc28zxzgli68k6j0dakh0k3s389bm436yvqajxg19xd"))))
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
             (patches (search-patches "clucene-pkgconfig.patch"
                                      "clucene-contribs-lib.patch"))))
    (build-system cmake-build-system)
    (inputs
     `(("boost" ,boost) ; could also use bundled copy
       ("zlib" ,zlib)))
    (arguments
     `(#:test-target "cl_test"
       #:configure-flags '("-DBUILD_CONTRIBS_LIB=ON")
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

(define-public lucene++
  (package
    (name "lucene++")
    (version "3.0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/luceneplusplus/LucenePlusPlus")
                     (commit (string-append "rel_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06b37fly6l27zc6kbm93f6khfsv61w792j8xihfagpcm9cfz2zi1"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ;; CXX_FLAGS suggested in a closed issue on github:
       ;; https://github.com/luceneplusplus/LucenePlusPlus/issues/100
       (list "-Wno-dev" "-DCMAKE_CXX_FLAGS=-DBOOST_VARIANT_USE_RELAXED_GET_BY_DEFAULT"
             ;; Install in lib64 break rpath
             "-DCMAKE_INSTALL_LIBDIR:PATH=lib")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)))
    (home-page "https://github.com/luceneplusplus/LucenePlusPlus")
    (synopsis "Text search engine")
    (description "Lucene++ is an up to date C++ port of the popular Java
Lucene library, a high-performance, full-featured text search engine.")
    (license (list asl2.0 lgpl3+)))); either asl or lgpl.

(define-public lrdf
  (package
    (name "lrdf")
    (version "0.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/swh/LRDF.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00wzkfb8y0aqd519ypz067cq099dpc89w69zw8ln39vl6f9x2pd4"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-out-of-tree-references
           (lambda _
             ;; remove_test depends on an out-of-tree RDF file
             (substitute* "examples/Makefile.am"
               (("instances_test remove_test") "instances_test")
               (("\\$\\(TESTS\\) remove_test") "$(TESTS)"))
             #t))
         ;; The default bootstrap phase executes autogen.sh, which fails.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vif") #t)))))
    (inputs
     `(("raptor" ,raptor2)
       ("cyrus-sasl" ,cyrus-sasl)
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/swh/LRDF")
    (synopsis "Lightweight RDF library for accessing LADSPA plugin metadata")
    (description
     "LRDF is a library to make it easy to manipulate RDF files describing
LADSPA plugins.  It can also be used for general RDF manipulation.  It can
read RDF/XLM and N3 files and export N3 files, and it also has a light
taxonomic inference capability.")
    (license gpl2)))

(define-public rasqal
  (package
    (name "rasqal")
    (version "0.9.33")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.librdf.org/source/" name
                                 "-" version ".tar.gz"))
             (sha256
              (base32
               "0z6rrwn4jsagvarg8d5zf0j352kjgi33py39jqd29gbhcnncj939"))))
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
    (propagated-inputs
     `(("rasqal" ,rasqal))) ; in Requires.private field of .pc
    (inputs
     `(("bdb" ,bdb)))
    (home-page "http://librdf.org/")
    (synopsis "RDF library")
    (description "The Redland RDF Library (librdf) provides the RDF API
and triple stores.")
    (license lgpl2.1+))) ; or any choice of gpl2+ or asl2.0

(define-public serd
  (package
    (name "serd")
    (version "0.30.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.drobilla.net/serd-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1yyfyvc6kwagi5w43ljp1bbjdvdpmgpds74lmjxycm91bkx0xyvf"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-ldflags
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "LDFLAGS"
                    (string-append "-Wl,-rpath="
                                   (assoc-ref outputs "out") "/lib"))
            #t)))))
    (home-page "https://drobilla.net/software/serd/")
    (synopsis "Library for RDF syntax supporting Turtle and NTriples")
    (description
     "Serd is a lightweight C library for RDF syntax which supports reading
and writing Turtle and NTriples.  Serd is not intended to be a swiss-army
knife of RDF syntax, but rather is suited to resource limited or performance
critical applications (e.g. converting many gigabytes of NTriples to Turtle),
or situations where a simple reader/writer with minimal dependencies is
ideal (e.g. in LV2 implementations or embedded applications).")
    (license isc)))

(define-public sord
  (package
    (name "sord")
    (version "0.16.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.drobilla.net/sord-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "13fshxwpipjrvsah1m2jw1kf022z2q5vpw24bzcznglgvms13x89"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-ldflags
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "LDFLAGS"
                    (string-append "-Wl,-rpath="
                                   (assoc-ref outputs "out") "/lib"))
            #t)))))
    (inputs
     `(("serd" ,serd)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://drobilla.net/software/sord/")
    (synopsis "C library for storing RDF data in memory")
    (description
     "Sord is a lightweight C library for storing RDF data in memory.")
    (license isc)))

(define-public python-rdflib
  (package
    (name "python-rdflib")
    (version "4.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rdflib" version))
        (sha256
         (base32
          "0398c714znnhaa2x7v51b269hk20iz073knq2mvmqp2ma92z27fs"))))
    (build-system python-build-system)
    (arguments
     '(;; FIXME: Three test failures. Should be fixed next release.
       #:tests? #f))
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       ;; Run tests from the build directory so python3 only
       ;;       ;; sees the installed 2to3 version.
       ;;       (invoke "nosetests" "--where=./build/src"))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
      `(("python-html5lib" ,python-html5lib)
        ("python-isodate" ,python-isodate)
        ("python-pyparsing" ,python-pyparsing)))
    (home-page "https://github.com/RDFLib/rdflib")
    (synopsis "Python RDF library")
    (description
      "RDFLib is a Python library for working with RDF, a simple yet
powerful language for representing information.")
    (license (non-copyleft "file://LICENSE"
                           "See LICENSE in the distribution."))))

(define-public python2-rdflib
  (package-with-python2 python-rdflib))
