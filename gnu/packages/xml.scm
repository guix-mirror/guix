;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
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

(define-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public expat
  (package
    (name "expat")
    (version "2.2.1")
    (replacement expat-2.2.4)
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/expat/expat/"
                                 version "/expat-" version ".tar.bz2"))
             (sha256
              (base32
               "11c8jy1wvllvlk7xdc5cm8hdhg0hvs8j0aqy6s702an8wkdcls0q"))))
    (build-system gnu-build-system)
    (home-page "http://www.libexpat.org/")
    (synopsis "Stream-oriented XML parser library written in C")
    (description
     "Expat is an XML parser library written in C.  It is a
stream-oriented parser in which an application registers handlers for
things the parser might find in the XML document (like start tags).")
    (license license:expat)))

(define expat-2.2.4  ; Fix CVE-{2016-9063,2017-9233,2017-11742} & other issues.
  (package
    (inherit expat)
    (version "2.2.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/expat/expat/"
                                 version "/expat-" version ".tar.bz2"))
             (sha256
              (base32
               "17h1fb9zvqvf0sr78j211bngc6jpql5wzar8fg9b52jzjvdqbb83"))))))

(define-public libebml
  (package
    (name "libebml")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.matroska.org/downloads/"
                           name "/" name "-" version ".tar.bz2"))
       (sha256
        (base32
         "11zka6z9ncywyjr1gfm5cnii33ln7y3w6s86kiacchip2g7kw3f5"))))
    (build-system gnu-build-system)
    (home-page "https://www.matroska.org")
    (synopsis "C++ libary to parse EBML files")
    (description "libebml is a C++ library to read and write EBML (Extensible
Binary Meta Language) files.  EBML was designed to be a simplified binary
extension of XML for the purpose of storing and manipulating data in a
hierarchical form with variable field lengths.")
    (license license:lgpl2.1)))

(define-public libxml2
  (package
    (name "libxml2")
    (version "2.9.4")
    (replacement libxml2/fixed)
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://xmlsoft.org/libxml2/libxml2-"
                                 version ".tar.gz"))
             (patches (search-patches "libxml2-CVE-2016-4658.patch"
                                      "libxml2-CVE-2016-5131.patch"))
             (sha256
              (base32
               "0g336cr0bw6dax1q48bblphmchgihx9p1pjmxdnrd6sh3qci3fgz"))))
    (build-system gnu-build-system)
    (home-page "http://www.xmlsoft.org/")
    (synopsis "C parser for XML")
    (propagated-inputs `(("zlib" ,zlib))) ; libxml2.la says '-lz'.
    (native-inputs `(("perl" ,perl)))
    ;; $XML_CATALOG_FILES lists 'catalog.xml' files found in under the 'xml'
    ;; sub-directory of any given package.
    (native-search-paths (list (search-path-specification
                                (variable "XML_CATALOG_FILES")
                                (separator " ")
                                (files '("xml"))
                                (file-pattern "^catalog\\.xml$")
                                (file-type 'regular))))
    (search-paths native-search-paths)
    (description
     "Libxml2 is the XML C parser and toolkit developed for the Gnome
project (but it is usable outside of the Gnome platform).")
    (license license:x11)))

(define libxml2/fixed
  (package
    (inherit libxml2)
    (source
     (origin
       (inherit (package-source libxml2))
       (patches
        (append (origin-patches (package-source libxml2))
        (search-patches "libxml2-CVE-2017-0663.patch"
                        "libxml2-CVE-2017-7375.patch"
                        "libxml2-CVE-2017-7376.patch"
                        "libxml2-CVE-2017-9047+CVE-2017-9048.patch"
                        "libxml2-CVE-2017-9049+CVE-2017-9050.patch")))))))

(define-public python-libxml2
  (package (inherit libxml2)
    (name "python-libxml2")
    (build-system python-build-system)
    (arguments
     `(;; XXX: Tests are specified in 'Makefile.am', but not in 'setup.py'.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'build 'configure
          (lambda* (#:key inputs #:allow-other-keys)
            (chdir "python")
            (let ((glibc   (assoc-ref inputs ,(if (%current-target-system)
                                                  "cross-libc" "libc")))
                  (libxml2 (assoc-ref inputs "libxml2")))
              (substitute* "setup.py"
                ;; For 'libxml2/libxml/tree.h'.
                (("ROOT = r'/usr'")
                 (format #f "ROOT = r'~a'" libxml2))
                ;; For 'iconv.h'.
                (("/opt/include")
                 (string-append glibc "/include")))))))))
    (inputs `(("libxml2" ,libxml2)))
    (synopsis "Python bindings for the libxml2 library")))

(define-public python2-libxml2
  (package-with-python2 python-libxml2))

(define-public libxslt
  (package
    (name "libxslt")
    (replacement libxslt/fixed)
    (version "1.1.29")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://xmlsoft.org/libxslt/libxslt-"
                                 version ".tar.gz"))
             ;; XXX Oops, the patches field is redefined below, which means the
             ;; patch for CVE-2016-4738 was not used. Fixed in the definition of
             ;; libxslt/fixed below.
             ;(patches (search-patches "libxslt-CVE-2016-4738.patch"))
             (sha256
              (base32
               "1klh81xbm9ppzgqk339097i39b7fnpmlj8lzn8bpczl3aww6x5xm"))
             (patches (search-patches "libxslt-generated-ids.patch"))))
    (build-system gnu-build-system)
    (home-page "http://xmlsoft.org/XSLT/index.html")
    (synopsis "C library for applying XSLT stylesheets to XML documents")
    (inputs `(("libgcrypt" ,libgcrypt)
              ("libxml2" ,libxml2)
              ("python" ,python-minimal-wrapper)
              ("zlib" ,zlib)))
    (description
     "Libxslt is an XSLT C library developed for the GNOME project.  It is
based on libxml for XML parsing, tree manipulation and XPath support.")
    (license license:x11)))

(define libxslt/fixed
  (package
    (inherit libxslt)
    (source (origin
              (inherit (package-source libxslt))
              (patches (search-patches "libxslt-CVE-2016-4738.patch"
                                       "libxslt-CVE-2017-5029.patch"
                                       "libxslt-generated-ids.patch"))))))

(define-public perl-graph-readwrite
  (package
    (name "perl-graph-readwrite")
    (version "2.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/N/NE/NEILB/Graph-ReadWrite-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0jlsg64pmy6ka5q5gy851nnyfgjzvhyxc576bhns3vi2x5ng07mh"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-graph" ,perl-graph)
       ("perl-parse-yapp" ,perl-parse-yapp)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-writer" ,perl-xml-writer)))
    (home-page "http://search.cpan.org/dist/Graph-ReadWrite")
    (synopsis "Modules for reading and writing directed graphs")
    (description "This is a collection of perl classes for reading and writing
directed graphs in a variety of file formats.  The graphs are represented in
Perl using Jarkko Hietaniemi's @code{Graph} classes.

There are two base classes. @code{Graph::Reader} is the base class for classes
which read a graph file and create an instance of the Graph class.
@code{Graph::Writer} is the base class for classes which take an instance of
the @code{Graph} class and write it out in a specific file format.")
    (license license:perl-license)))

(define-public perl-xml-atom
  (package
    (name "perl-xml-atom")
    (version "0.42")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIYAGAWA/"
                                  "XML-Atom-" version ".tar.gz"))
              (sha256
               (base32
                "1wa8kfy1w4mg7kzxim4whyprkn48a2il6fap0b947zywknw4c6y6"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-perl-search-path
           (lambda _
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))
             #t)))))
    (native-inputs
     `(("perl-datetime" ,perl-datetime)
       ;; TODO package: perl-datetime-format-atom
       ("perl-html-tagset" ,perl-html-tagset)
       ("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-module-install" ,perl-module-install)
       ("perl-xml-xpath" ,perl-xml-xpath)))
    (inputs
     `(("perl-class-data-inheritable" ,perl-class-data-inheritable)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-digest-sha1" ,perl-digest-sha1)
       ("perl-libwww" ,perl-libwww)
       ("perl-uri" ,perl-uri)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-xml-xpath" ,perl-xml-xpath)))
    (home-page "http://search.cpan.org/dist/XML-Atom")
    (synopsis "Atom feed and API implementation")
    (description
     "Atom is a syndication, API, and archiving format for weblogs and other data.
@code{XML::Atom} implements the feed format as well as a client for the API.")
    (license license:perl-license)))

(define-public perl-xml-descent
  (package
    (name "perl-xml-descent")
    (version "1.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AN/ANDYA/"
                                  "XML-Descent-" version ".tar.gz"))
              (sha256
               (base32
                "0l5xmw2hd95ypppz3lyvp4sn02ccsikzjwacli3ydxfdz1bbh4d7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-test-differences" ,perl-test-differences)
       ("perl-xml-tokeparser" ,perl-xml-tokeparser)))
    (home-page "http://search.cpan.org/dist/XML-Descent")
    (synopsis "Recursive descent XML parsing")
    (description
     "The conventional models for parsing XML are either @dfn{DOM}
(a data structure representing the entire document tree is created) or
@dfn{SAX} (callbacks are issued for each element in the XML).

XML grammar is recursive - so it's nice to be able to write recursive
parsers for it.  @code{XML::Descent} allows such parsers to be created.")
    (license license:perl-license)))

(define-public perl-xml-parser
  (package
    (name "perl-xml-parser")
    (version "2.44")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TO/TODDR/XML-Parser-"
                   version ".tar.gz"))
             (sha256
              (base32
               "05ij0g6bfn27iaggxf8nl5rhlwx6f6p6xmdav6rjcly3x5zd1s8s"))))
    (build-system perl-build-system)
    (arguments `(#:make-maker-flags
                 (let ((expat (assoc-ref %build-inputs "expat")))
                   (list (string-append "EXPATLIBPATH=" expat "/lib")
                         (string-append "EXPATINCPATH=" expat "/include")))))
    (inputs `(("expat" ,expat)))
    (license license:perl-license)
    (synopsis "Perl bindings to the Expat XML parsing library")
    (description
     "This module provides ways to parse XML documents.  It is built on top of
XML::Parser::Expat, which is a lower level interface to James Clark's expat
library.  Each call to one of the parsing methods creates a new instance of
XML::Parser::Expat which is then used to parse the document.  Expat options
may be provided when the XML::Parser object is created.  These options are
then passed on to the Expat object on each parse call.  They can also be given
as extra arguments to the parse methods, in which case they override options
given at XML::Parser creation time.")
    (home-page "http://search.cpan.org/dist/XML-Parser")))

(define-public perl-xml-tokeparser
  (package
    (name "perl-xml-tokeparser")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/P/PO/PODMASTER/"
                                  "XML-TokeParser-" version ".tar.gz"))
              (sha256
               (base32
                "1hnpwb3lh6cbgwvjjgqzcp6jm4mp612qn6ili38adc9nhkwv8fc5"))))
    (build-system perl-build-system)
    (propagated-inputs `(("perl-xml-parser" ,perl-xml-parser)))
    (home-page "http://search.cpan.org/dist/XML-TokeParser")
    (synopsis "Simplified interface to XML::Parser")
    (description
     "@code{XML::TokeParser} provides a procedural (\"pull mode\") interface
to @code{XML::Parser} in much the same way that Gisle Aas'
@code{HTML::TokeParser} provides a procedural interface to @code{HTML::Parser}.
@code{XML::TokeParser} splits its XML input up into \"tokens\", each
corresponding to an @code{XML::Parser} event.")
    (license license:perl-license)))

(define-public perl-libxml
  (package
    (name "perl-libxml")
    (version "0.08")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/K/KM/KMACLEOD/libxml-perl-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1jy9af0ljyzj7wakqli0437zb2vrbplqj4xhab7bfj2xgfdhawa5"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-parser" ,perl-xml-parser)))
    (license license:perl-license)
    (synopsis "Perl SAX parser using XML::Parser")
    (description
     "XML::Parser::PerlSAX is a PerlSAX parser using the XML::Parser
module.")
    (home-page "http://search.cpan.org/~kmacleod/libxml-perl/lib/XML/Parser/PerlSAX.pm")))

(define-public perl-xml-libxml
  (package
    (name "perl-xml-libxml")
    (version "2.0132")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "XML-LibXML-" version ".tar.gz"))
       (sha256
        (base32
         "0xnl281hb590i287fxpl947f1s4zl9dnvc4ajvsqi89w23im453j"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-namespacesupport" ,perl-xml-namespacesupport)
       ("perl-xml-sax" ,perl-xml-sax)))
    (inputs
     `(("libxml2" ,libxml2)))
    (home-page "http://search.cpan.org/dist/XML-LibXML")
    (synopsis "Perl interface to libxml2")
    (description "This module implements a Perl interface to the libxml2
library which provides interfaces for parsing and manipulating XML files.  This
module allows Perl programmers to make use of the highly capable validating
XML parser and the high performance DOM implementation.")
    (license license:perl-license)))

(define-public perl-xml-libxml-simple
  (package
    (name "perl-xml-libxml-simple")
    (version "0.97")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-LibXML-Simple-" version ".tar.gz"))
              (sha256
               (base32
                "1g8nlk3zdz2cclxf7azvsb3jfxmvy6ml8wmj774k4qjqcsqmzk0w"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-slurp-tiny" ,perl-file-slurp-tiny)
       ("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "http://search.cpan.org/dist/XML-LibXML-Simple")
    (synopsis "XML::LibXML based XML::Simple clone")
    (description
     "This package provides the same API as @code{XML::Simple} but is based on
@code{XML::LibXML}.")
    (license license:perl-license)))

(define-public perl-xml-libxslt
  (package
    (name "perl-xml-libxslt")
    (version "1.95")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "XML-LibXSLT-" version ".tar.gz"))
       (sha256
        (base32
         "0dggycql18kfxzkb1kw3yc7gslxlrrgyyn2r2ygsylycb89j3jpi"))))
    (build-system perl-build-system)
    (inputs
     `(("libxslt" ,libxslt)))
    (propagated-inputs
     `(("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "http://search.cpan.org/dist/XML-LibXSLT")
    (synopsis "Perl bindings to GNOME libxslt library")
    (description "This Perl module is an interface to the GNOME project's
libxslt library.")
    (license license:perl-license)))

(define-public perl-xml-namespacesupport
  (package
    (name "perl-xml-namespacesupport")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PE/PERIGRIN/"
                           "XML-NamespaceSupport-" version ".tar.gz"))
       (sha256
        (base32
         "1vz5pbi4lm5fhq2slrs2hlp6bnk29863abgjlcx43l4dky2rbsa7"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/XML-NamespaceSupport")
    (synopsis "XML namespace support class")
    (description "This module offers a simple to process namespaced XML
names (unames) from within any application that may need them.  It also helps
maintain a prefix to namespace URI map, and provides a number of basic
checks.")
    (license license:perl-license)))

(define-public perl-xml-rss
  (package
    (name "perl-xml-rss")
    (version "1.59")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                                  "XML-RSS-" version ".tar.gz"))
              (sha256
               (base32
                "0v6vfizn2psy6av057kp7fv3z3y73s6b3w56jm3zr6hlq48llsx2"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-manifest" ,perl-test-manifest)
       ("perl-test-differences" ,perl-test-differences)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    ;; XXX: The test which uses this modules does not run, even when it is included
    ;; it is ignored. ("perl-test-trailingspace" ,perl-test-trailingspace)
    (inputs
     `(("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-mail" ,perl-datetime-format-mail)
       ("perl-datetime-format-w3cdtf" ,perl-datetime-format-w3cdtf)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-xml-parser" ,perl-xml-parser)))
    (home-page "http://search.cpan.org/dist/XML-RSS")
    (synopsis "Creates and updates RSS files")
    (description
     "This module provides a basic framework for creating and maintaining
RDF Site Summary (RSS) files.  This distribution also contains many examples
that allow you to generate HTML from an RSS, convert between 0.9, 0.91, and
1.0 version, and more.")
    (license license:perl-license)))

(define-public perl-xml-sax
  (package
    (name "perl-xml-sax")
    (version "0.99")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRANTM/"
                           "XML-SAX-" version ".tar.gz"))
       (sha256
        (base32
         "115dypb50w1l94y3iwihv5nkixbsv1cxiqkd93y4rk5n6s74pc1j"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-namespacesupport" ,perl-xml-namespacesupport)
       ("perl-xml-sax-base" ,perl-xml-sax-base)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'install 'augment-path
                   ;; The install target tries to load the newly-installed
                   ;; XML::SAX module, but can't find it, so we need to tell
                   ;; perl where to look.
                   (lambda* (#:key outputs #:allow-other-keys)
                     (setenv "PERL5LIB"
                             (string-append (getenv "PERL5LIB") ":"
                                            (assoc-ref outputs "out")
                                            "/lib/perl5/site_perl")))))))
    (home-page "http://search.cpan.org/dist/XML-SAX")
    (synopsis "Perl API for XML")
    (description "XML::SAX consists of several framework classes for using and
building Perl SAX2 XML parsers, filters, and drivers.")
    (license license:perl-license)))

(define-public perl-xml-sax-base
  (package
    (name "perl-xml-sax-base")
    (version "1.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRANTM/"
                           "XML-SAX-Base-" version ".tar.gz"))
       (sha256
        (base32
         "17i161rq1ngjlk0c8vdkrkkc56y1pf51k1g54y28py0micqp0qk6"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/XML-SAX-Base")
    (synopsis "Base class for SAX Drivers and Filters")
    (description "This module has a very simple task - to be a base class for
PerlSAX drivers and filters.  It's default behaviour is to pass the input
directly to the output unchanged.  It can be useful to use this module as a
base class so you don't have to, for example, implement the characters()
callback.")
    (license license:perl-license)))

(define-public perl-xml-simple
  (package
    (name "perl-xml-simple")
    (version "2.22")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GR/GRANTM/XML-Simple-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0jgbk30jizafpl7078jhw1di1yh08gf8d85dsvjllr595vr0widr"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-sax" ,perl-xml-sax)))
    (license license:perl-license)
    (synopsis "Perl module for easy reading/writing of XML files")
    (description
     "The XML::Simple module provides a simple API layer on top of an
underlying XML parsing module (either XML::Parser or one of the SAX2
parser modules).")
    (home-page "http://search.cpan.org/dist/XML-Simple")))

(define-public perl-xml-regexp
  (package
    (name "perl-xml-regexp")
    (version "0.04")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TJ/TJMATHER/XML-RegExp-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0m7wj00a2kik7wj0azhs1zagwazqh3hlz4255n75q21nc04r06fz"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-xml-parser" ,perl-xml-parser)))
    (license license:perl-license)
    (synopsis "Perl regular expressions for XML tokens")
    (description
     "XML::RegExp contains regular expressions for the following XML tokens:
BaseChar, Ideographic, Letter, Digit, Extender, CombiningChar, NameChar,
EntityRef, CharRef, Reference, Name, NmToken, and AttValue.")
    (home-page "http://search.cpan.org/~tjmather/XML-RegExp/lib/XML/RegExp.pm")))

(define-public perl-xml-dom
  (package
    (name "perl-xml-dom")
    (version "1.46")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/T/TJ/TJMATHER/XML-DOM-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0phpkc4li43m2g44hdcvyxzy9pymqwlqhh5hwp2xc0cv8l5lp8lb"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libwww" ,perl-libwww)
       ("perl-libxml" ,perl-libxml)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-regexp" ,perl-xml-regexp)))
    (license license:perl-license)
    (synopsis
     "Perl module for building DOM Level 1 compliant document structures")
    (description
     "This module extends the XML::Parser module by Clark Cooper.  The
XML::Parser module is built on top of XML::Parser::Expat, which is a lower
level interface to James Clark's expat library.  XML::DOM::Parser is derived
from XML::Parser.  It parses XML strings or files and builds a data structure
that conforms to the API of the Document Object Model.")
    (home-page "http://search.cpan.org/~tjmather/XML-DOM-1.44/lib/XML/DOM.pm")))

(define-public perl-xml-compile-tester
  (package
    (name "perl-xml-compile-tester")
    (version "0.90")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-Tester-" version ".tar.gz"))
              (sha256
               (base32
                "1bcl8x8cyacqv9yjp97aq9qq85sy8wv78kd8c16yd9yw3by4cpp1"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-test-deep" ,perl-test-deep)))
    (home-page "http://search.cpan.org/dist/XML-Compile-Tester")
    (synopsis "XML::Compile related regression testing")
    (description
     "The @code{XML::Compile} module suite has extensive regression testing.
This module provide functions which simplify writing tests for
@code{XML::Compile} related distributions.")
    (license license:perl-license)))

(define-public perl-xml-compile
  (package
    (name "perl-xml-compile")
    (version "1.54")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-" version ".tar.gz"))
              (sha256
               (base32
                "1hp41960bpqxvv1samv9hc0ghhmvs3i16r4rfl9yp54lp6jhsr2c"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-test-deep" ,perl-test-deep)))
    (home-page "http://search.cpan.org/dist/XML-Compile")
    (synopsis "Compilation-based XML processing")
    (description
     "@code{XML::Compile} can be used to translate a Perl data-structure into
XML or XML into a Perl data-structure, both directions under rigid control by
a schema.")
    (license license:perl-license)))

(define-public perl-xml-compile-cache
  (package
    (name "perl-xml-compile-cache")
    (version "1.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-Cache-" version ".tar.gz"))
              (sha256
               (base32
                "0xbwlszhi9hg8sxm5ylglm2qvnb689i595p913awrj2g4mp9yfsw"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)
       ("perl-xml-libxml-simple" ,perl-xml-libxml-simple)))
    (home-page "http://search.cpan.org/dist/XML-Compile-Cache")
    (synopsis "Cache compiled XML translators")
    (description
     "This package provides methods to cache compiled XML translators.")
    (license license:perl-license)))

(define-public perl-xml-compile-soap
  (package
    (name "perl-xml-compile-soap")
    (version "3.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-SOAP-" version ".tar.gz"))
              (sha256
               (base32
                "0rxidh7kjyhnw2y789bqbwccnp8n0m3xskn524y9c752s64qpjcz"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-slurp-tiny" ,perl-file-slurp-tiny)
       ("perl-libwww" ,perl-libwww)
       ("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-cache" ,perl-xml-compile-cache)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)))
    (home-page "http://search.cpan.org/dist/XML-Compile-SOAP")
    (synopsis "Base-class for SOAP implementations")
    (description
     "This module provides a class to handle the SOAP protocol.  The first
implementation is @url{SOAP1.1,
http://www.w3.org/TR/2000/NOTE-SOAP-20000508/}, which is still most often
used.")
    (license license:perl-license)))

(define-public perl-xml-compile-wsdl11
  (package
    (name "perl-xml-compile-wsdl11")
    (version "3.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-WSDL11-" version ".tar.gz"))
              (sha256
               (base32
                "0vbq05cpynm3jj81fw1k4nsb3wv4zngi6blvi1jhdarmh2rfg1x2"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-cache" ,perl-xml-compile-cache)
       ("perl-xml-compile-soap" ,perl-xml-compile-soap)))
    (home-page "http://search.cpan.org/dist/XML-Compile-WSDL11")
    (synopsis "Create SOAP messages defined by WSDL 1.1")
    (description
     "This module understands WSDL version 1.1.  A WSDL file defines a set of
messages to be send and received over SOAP connections.  This involves
encoding of the message to be send into XML, sending the message to the
server, collect the answer, and finally decoding the XML to Perl.")
    (license license:perl-license)))

(define-public perl-xml-feed
  (package
    (name "perl-xml-feed")
    (version "0.53")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DA/DAVECROSS/"
                                  "XML-Feed-" version ".tar.gz"))
              (sha256
               (base32
                "07b165g6wk8kqwpl49r3n0kag6p2nrkyp3ch0h8qyxb6nrnkkq7c"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require internet connection
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-uri" ,perl-uri)
       ("perl-class-data-inheritable" ,perl-class-data-inheritable)))
    (inputs
     `(("perl-class-errorhandler" ,perl-class-errorhandler)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-format-mail" ,perl-datetime-format-mail)
       ("perl-datetime-format-w3cdtf" ,perl-datetime-format-w3cdtf)
       ("perl-feed-find" ,perl-feed-find)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-libwww-perl" ,perl-libwww)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-uri-fetch" ,perl-uri-fetch)
       ("perl-xml-atom" ,perl-xml-atom)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-xml-rss" ,perl-xml-rss)))
    (home-page "http://search.cpan.org/dist/XML-Feed")
    (synopsis "XML Syndication Feed Support")
    (description "@code{XML::Feed} is a syndication feed parser for both RSS and
Atom feeds.  It also implements feed auto-discovery for finding feeds, given a URI.
@code{XML::Feed} supports the following syndication feed formats:
RSS 0.91, RSS 1.0, RSS 2.0, Atom")
    (license license:perl-license)))

(define-public perl-xml-xpath
  (package
    (name "perl-xml-xpath")
    (version "1.40")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MANWAR/"
                                  "XML-XPath-" version ".tar.gz"))
              (sha256
               (base32
                "07pa0bl42jka8mj7jshjynx8vpfh8b4cdyiv4zlkqvkqz98nzxib"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-path-tiny" ,perl-path-tiny)))
    (inputs
     `(("perl-xml-parser" ,perl-xml-parser)))
    (home-page "http://search.cpan.org/dist/XML-XPath")
    (synopsis "Parse and evaluate XPath statements")
    (description
     "This module aims to comply exactly to the @url{XPath specification,
https://www.w3.org/TR/xpath} and yet allow extensions to be added in
the form of functions.")
    (license license:perl-license)))

(define-public pugixml
  (package
    (name "pugixml")
    (version "1.8.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/zeux/pugixml/releases/download/v"
                          version "/pugixml-" version ".tar.gz"))
      (sha256
       (base32
        "0fcgggry5x5bn0zhb09ij9hb0p45nb0sv0d9fw3cm1cf62hp9n80"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DCMAKE_CXX_FLAGS=-shared -fPIC"
                           "-DCMAKE_C_FLAGS=-shared -fPIC")
       #:tests? #f))                    ; no tests
    (home-page "http://pugixml.org")
    (synopsis "Light-weight, simple and fast XML parser for C++ with XPath support")
    (description
     "pugixml is a C++ XML processing library, which consists of a DOM-like
interface with rich traversal/modification capabilities, a fast XML parser
which constructs the DOM tree from an XML file/buffer, and an XPath 1.0
implementation for complex data-driven tree queries.  Full Unicode support is
also available, with Unicode interface variants and conversions between
different Unicode encodings which happen automatically during
parsing/saving.")
    (license license:expat)))

(define-public python-pyxb
  (package
    (name "python-pyxb")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyXB" version))
              (sha256
               (base32
                "1d17pyixbfvjyi2lb0cfp0ch8wwdf44mmg3r5pwqhyyqs66z601a"))))
    (build-system python-build-system)
    (home-page "http://pyxb.sourceforge.net/")
    (synopsis "Python XML Schema Bindings")
    (description
     "PyXB (\"pixbee\") is a pure Python package that generates Python source
code for classes that correspond to data structures defined by XMLSchema.")
    (license (list license:asl2.0    ; Most files.
                   license:expat     ; pyxb/utils/six.py
                   license:gpl2      ; bundled jquery in doc is dual MIT/GPL2
                   license:psfl))))  ; pyxb/utils/activestate.py

(define-public python2-pyxb
  (package-with-python2 python-pyxb))

(define-public xmlto
  (package
    (name "xmlto")
    (version "0.0.28")
    (source
     (origin
      (method url-fetch)
      ;; The old source on fedorahosted.org is offline permanently:
      ;; <https://bugs.gnu.org/25989>
      (uri (string-append "mirror://debian/pool/main/x/xmlto/"
                          "xmlto_" version ".orig.tar.bz2"))
      (file-name (string-append name "-" version ".tar.bz2"))
      (sha256
       (base32
        "0xhj8b2pwp4vhl9y16v3dpxpsakkflfamr191mprzsspg4xdyc0i"))))
    (build-system gnu-build-system)
    (arguments
     ;; Make sure the reference to util-linux's 'getopt' is kept in 'xmlto'.
     '(#:configure-flags (list (string-append "GETOPT="
                                              (assoc-ref %build-inputs
                                                         "util-linux")
                                              "/bin/getopt"))))
    (inputs
     `(("util-linux" ,util-linux)                 ; for 'getopt'
       ("libxml2" ,libxml2)                       ; for 'xmllint'
       ("libxslt" ,libxslt)))                     ; for 'xsltproc'
    (home-page "http://cyberelk.net/tim/software/xmlto/")
    (synopsis "Front-end to an XSL toolchain")
    (description
     "Xmlto is a front-end to an XSL toolchain.  It chooses an appropriate
stylesheet for the conversion you want and applies it using an external
XSL-T processor.  It also performs any necessary post-processing.")
    (license license:gpl2+)))

(define-public xmlsec
  (package
    (name "xmlsec")
    (version "1.2.20")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.aleksey.com/xmlsec/download/"
                                 name "1-" version ".tar.gz"))
             (sha256
              (base32
               "01bkbv2y3x8d1sf4dcln1x3y2jyj391s3208d9a2ndhglly5j89j"))))
    (build-system gnu-build-system)
    (propagated-inputs ; according to xmlsec1.pc
     `(("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (inputs
     `(("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("libltdl" ,libltdl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.libexpat.org/")
    (synopsis "XML Security Library")
    (description
     "The XML Security Library is a C library based on Libxml2.  It
supports XML security standards such as XML Signature, XML Encryption,
Canonical XML (part of Libxml2) and Exclusive Canonical XML (part of
Libxml2).")
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public minixml
  (package
    (name "minixml")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/michaelrsweet/mxml/"
                                  "releases/download/release-" version
                                  "/mxml-" version ".tar.gz"))
              (sha256
               (base32
                "14bqfq4lymhb31snz6wsvzhlavy0573v1nki1lbngiyxcj5zazr6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))  ;no "check" target
    (home-page "https://michaelrsweet.github.io/mxml")
    (synopsis "Small XML parsing library")
    (description
     "Mini-XML is a small C library to read and write XML files and strings in
UTF-8 and UTF-16 encoding.")
    ;; LGPL 2.0+ with additional exceptions for static linking
    (license license:lgpl2.0+)))

;; TinyXML is an unmaintained piece of software, so the patches and build
;; system massaging have no upstream potential.
(define-public tinyxml
  (package
    (name "tinyxml")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/tinyxml/tinyxml/"
                                  version "/tinyxml_"
                                  (string-join (string-split version #\.) "_")
                                  ".tar.gz"))
              (sha256
               (base32
                "14smciid19lvkxqznfig77jxn5s4iq3jpb47vh5a6zcaqp7gvg8m"))
              (patches (search-patches "tinyxml-use-stl.patch"))))
    (build-system gnu-build-system)
    ;; This library is missing *a lot* of the steps to make it usable, so we
    ;; have to add them here, like every other distro must do.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'build-shared-library
           (lambda _
             (zero? (system* "g++" "-Wall" "-O2" "-shared" "-fpic"
                             "tinyxml.cpp" "tinyxmlerror.cpp"
                             "tinyxmlparser.cpp" "tinystr.cpp"
                             "-o" "libtinyxml.so"))))
         (replace 'check
           (lambda _ (zero? (system "./xmltest"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include"))
                    (lib (string-append out "/lib"))
                    (pkgconfig (string-append out "/lib/pkgconfig"))
                    (doc (string-append out "/share/doc")))
               ;; Install libs and headers.
               (install-file "libtinyxml.so" lib)
               (install-file "tinystr.h" include)
               (install-file "tinyxml.h" include)
               ;; Generate and install pkg-config file.
               (mkdir-p pkgconfig)
               ;; Software such as Kodi expect this file to be present, but
               ;; it's not provided in the source code.
               (call-with-output-file (string-append pkgconfig "/tinyxml.pc")
                 (lambda (port)
                   (format port "prefix=~a
exec_prefix=${prefix}
libdir=${exec_prefix}/lib
includedir=${prefix}/include

Name: TinyXML
Description: A simple, small, C++ XML parser
Version: ~a
Libs: -L${libdir} -ltinyxml
Cflags: -I${includedir}
"
                           out ,version)))
               ;; Install docs.
               (mkdir-p doc)
               (copy-recursively "docs" (string-append doc "tinyxml"))
               #t))))))
    (synopsis "Small XML parser for C++")
    (description "TinyXML is a small and simple XML parsing library for the
C++ programming language.")
    (home-page "http://www.grinninglizard.com/tinyxml/index.html")
    (license license:zlib)))

(define-public tinyxml2
  (package
    (name "tinyxml2")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/leethomason/tinyxml2/archive/"
                           version ".tar.gz"))
       (sha256
       (base32
        "083z4r4khcndxi9k840lcr48sqxvar4gpsnf749xfdn1bkr8xcql"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))    ; no tests
    (synopsis "Small XML parser for C++")
    (description "TinyXML2 is a small and simple XML parsing library for the
C++ programming language.")
    (home-page "http://www.grinninglizard.com/tinyxml2/")
    (license license:zlib)))

(define-public xmlstarlet
 (package
   (name "xmlstarlet")
   (version "1.6.1")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/xmlstar/xmlstarlet/"
                          version "/xmlstarlet-" version ".tar.gz"))
      (sha256
       (base32
        "1jp737nvfcf6wyb54fla868yrr39kcbijijmjpyk4lrpyg23in0m"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases
     (modify-phases %standard-phases
       (add-before 'check 'drop-failing-tests
         (lambda _
           ;; FIXME: Why are these tests failing.
           (substitute* "Makefile"
             (("^examples/schema1\\\\") "\\")
             (("^examples/valid1\\\\") "\\"))
           #t)))))
   (inputs
    `(("libxslt" ,libxslt)
      ("libxml2" ,libxml2)))
   (home-page "http://xmlstar.sourceforge.net/")
   (synopsis "Command line XML toolkit")
   (description "XMLStarlet is a set of command line utilities which can be
used to transform, query, validate, and edit XML documents.  XPath is used to
match and extract data, and elements can be added, deleted or modified using
XSLT and EXSLT.")
   (license license:x11)))

(define-public xlsx2csv
  (package
    (name "xlsx2csv")
    (version "0.7.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/dilshod/"
                   name "/archive/release/" version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1gpn6kaa7l1ai8c9zx2j3acf04bvxq79pni8jjfjrk01smjbyyql"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; Use python-2 for the test script.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (substitute* "test/run"
               ;; Run tests with `python' only
               (("^(PYTHON_VERSIONS = ).*" all m) (string-append m "['']")))
             (zero? (system* "test/run")))))))
    (home-page "https://github.com/dilshod/xlsx2csv")
    (synopsis "XLSX to CSV converter")
    (description
     "Xlsx2csv is a program to convert Microsoft Excel 2007 XML (XLSX and
XLSM) format spreadsheets into plaintext @dfn{comma separated values} (CSV)
files.  It is designed to be fast and to handle large input files.")
    (license license:gpl2+)))

(define-public python-defusedxml
  (package
    (name "python-defusedxml")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "defusedxml" version))
       (sha256
        (base32
         "0y147zy3jqmk6ly7fbhqmzn1hf41xcb53f2vcc3m8x4ba5d1smfd"))))
    (build-system python-build-system)
    (home-page "https://bitbucket.org/tiran/defusedxml")
    (synopsis "XML bomb protection for Python stdlib modules")
    (description
     "Defusedxml provides XML bomb protection for Python stdlib modules.")
    (license license:psfl)))

(define-public python2-defusedxml
  (package-with-python2 python-defusedxml))

(define-public libxls
  (package
    (name "libxls")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceforge.net/projects/"
                                  name "/files/" name "-"
                                  version ".zip"))
              (sha256
               (base32
                "1g8ds7wbhsa4hdcn77xc2c0l3vvz5bx2hx9ng9c9n7aii92ymfnk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Bootstrapping is required in order to fix the test driver script.
         (add-after 'unpack 'bootstrap
           (lambda _
             (zero? (system* "bash" "bootstrap")))))))
    (native-inputs
     `(("unzip" ,unzip)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "http://libxls.sourceforge.net/")
    (synopsis "Read Excel files")
    (description
     "libxls is a C library which can read Excel (xls) files since Excel 97 (the BIFF8 format).
libxls cannot write Excel files.")
    (license license:bsd-2)))

(define-public freexl
  (package
    (name "freexl")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.gaia-gis.it/gaia-sins/"
                                  name  "-" version ".tar.gz"))
              (sha256
               (base32
                "09bwzqjc41cc8qw8qkw9wq58rg9nax8r3fg19iny5vmw1c0z23sh"))))
    (build-system gnu-build-system)
    (home-page "https://www.gaia-gis.it/fossil/freexl/index")
    (synopsis "Read Excel files")
    (description
     "FreeXL is a C library to extract valid data from within an Excel (.xls)
spreadsheet.")
    ;; Any of these licenses may be picked.
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1))))

(define-public xerces-c
  (package
    (name "xerces-c")
    (version "3.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xerces/c/3/sources/"
                                  "xerces-c-" version ".tar.xz"))
              (sha256
               (base32
                "0hb29c0smqlpxj0zdm09s983z5jx37szlliccnvgh0qq91wwqwwr"))))
    (build-system gnu-build-system)
    (arguments
     (let ((system (or (%current-target-system)
                       (%current-system))))
       (if (string-prefix? "x86_64" system)
           '()
           '(#:configure-flags '("--disable-sse2")))))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "http://xerces.apache.org/xerces-c/")
    (synopsis "Validating XML parser library for C++")
    (description "Xerces-C++ is a validating XML parser written in a portable
subset of C++.  Xerces-C++ makes it easy to give your application the ability
to read and write XML data.  A shared library is provided for parsing,
generating, manipulating, and validating XML documents using the DOM, SAX, and
SAX2 APIs.")
    (license license:asl2.0)))

(define-public java-simple-xml
  (package
    (name "java-simple-xml")
    (version "2.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/simple/simple-xml-"
                                  version ".zip"))
              (sha256
               (base32
                "0w19k1awslmihpwsxwjbg89hv0vjhk4k3i0vrfchy3mqknd988y5"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "build"
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "jar")))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://simple.sourceforge.net/")
    (synopsis "XML serialization framework for Java")
    (description "Simple is a high performance XML serialization and
configuration framework for Java.  Its goal is to provide an XML framework
that enables rapid development of XML configuration and communication systems.
This framework aids the development of XML systems with minimal effort and
reduced errors.  It offers full object serialization and deserialization,
maintaining each reference encountered.")
    (license license:asl2.0)))

(define-public perl-xml-xpathengine
  (package
    (name "perl-xml-xpathengine")
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "XML-XPathEngine-" version ".tar.gz"))
              (sha256
               (base32
                "0r72na14bmsxfd16s9nlza155amqww0k8wsa9x2a3sqbpp5ppznj"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/XML-XPathEngine/")
    (synopsis "Re-usable XPath engine for DOM-like trees")
    (description
     "This module provides an XPath engine, that can be re-used by other
modules/classes that implement trees.

In order to use the XPath engine, nodes in the user module need to mimick DOM
nodes.  The degree of similitude between the user tree and a DOM dictates how
much of the XPath features can be used.  A module implementing all of the DOM
should be able to use this module very easily (you might need to add the
@code{cmp} method on nodes in order to get ordered result sets).")
    (license license:perl-license)))

(define-public perl-tree-xpathengine
  (package
    (name "perl-tree-xpathengine")
    (version "0.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "Tree-XPathEngine-" version ".tar.gz"))
              (sha256
               (base32
                "1vbbw8wxm79r3xbra8narw1dqvm34510q67wbmg2zmj6zd1k06r9"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Tree-XPathEngine/")
    (synopsis "Re-usable XPath engine")
    (description
     "This module provides an XPath engine, that can be re-used by other
module/classes that implement trees.  It is designed to be compatible with
@code{Class::XPath}, ie it passes its tests if you replace @code{Class::XPath}
by @code{Tree::XPathEngine}.")
    (license license:perl-license)))

(define-public perl-xml-filter-buffertext
  (package
    (name "perl-xml-filter-buffertext")
    (version "1.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RB/RBERJON/"
                           "XML-Filter-BufferText-" version ".tar.gz"))
       (sha256
        (base32
         "0p5785c1dsk6kdp505vapb5h54k8krrz8699hpgm9igf7dni5llg"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-sax-base" ,perl-xml-sax-base)))
    (home-page "http://search.cpan.org/dist/XML-Filter-BufferText/")
    (synopsis "Filter to put all characters() in one event")
    (description "This is a very simple filter.  One common cause of
grief (and programmer error) is that XML parsers aren't required to provide
character events in one chunk.  They can, but are not forced to, and most
don't.  This filter does the trivial but oft-repeated task of putting all
characters into a single event.")
    (license license:perl-license)))

(define-public perl-xml-sax-writer
  (package
    (name "perl-xml-sax-writer")
    (version "0.57")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/P/PE/PERIGRIN/"
                    "XML-SAX-Writer-" version ".tar.gz"))
              (sha256
               (base32
                "1w1cd1ybxdvhmnxdlkywi3x5ka3g4md42kyynksjc09vyizd0q9x"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libxml" ,perl-libxml)
       ("perl-xml-filter-buffertext" ,perl-xml-filter-buffertext)
       ("perl-xml-namespacesupport", perl-xml-namespacesupport)
       ("perl-xml-sax-base" ,perl-xml-sax-base)))
    (home-page "http://search.cpan.org/dist/XML-SAX-Writer/")
    (synopsis "SAX2 XML Writer")
    (description
     "This is an XML writer that understands SAX2.  It is based on
@code{XML::Handler::YAWriter}.")
    (license license:perl-license)))

(define-public perl-xml-handler-yawriter
  (package
    (name "perl-xml-handler-yawriter")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KR/KRAEHE/"
                           "XML-Handler-YAWriter-" version ".tar.gz"))
       (sha256
        (base32
         "11d45a1sz862va9rry3p2m77pwvq3kpsvgwhc5ramh9mbszbnk77"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-libxml" ,perl-libxml)))
    (home-page "http://search.cpan.org/dist/XML-Handler-YAWriter/")
    (synopsis "Yet another Perl SAX XML Writer")
    (description "YAWriter implements Yet Another @code{XML::Handler::Writer}.
It provides a flexible escaping technique and pretty printing.")
    (license license:perl-license)))

(define-public perl-xml-twig
  (package
    (name "perl-xml-twig")
    (version "3.52")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIROD/"
                                  "XML-Twig-" version ".tar.gz"))
              (sha256
               (base32
                "1bc0hrz4jp6199hi29sdxmb9gyy45whla9hd19yqfasgq8k5ixzy"))))
    (build-system perl-build-system)
    (inputs
     `(("expat" ,expat)))
    (propagated-inputs
     `(("perl-html-tidy" ,perl-html-tidy)
       ("perl-html-tree" ,perl-html-tree)
       ("perl-io-captureoutput" ,perl-io-captureoutput)
       ("perl-io-string" ,perl-io-string)
       ("perl-io-stringy" ,perl-io-stringy)
       ("perl-libxml" ,perl-libxml)
       ("perl-xml-filter-buffertext" ,perl-xml-filter-buffertext)
       ("perl-xml-handler-yawriter" ,perl-xml-handler-yawriter)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-sax-writer" ,perl-xml-sax-writer)
       ("perl-xml-simple" ,perl-xml-simple)
       ("perl-xml-xpathengine" ,perl-xml-xpathengine)
       ("perl-test-pod", perl-test-pod)
       ("perl-tree-xpathengine" ,perl-tree-xpathengine)))
    (home-page "http://search.cpan.org/dist/XML-Twig/")
    (synopsis "Perl module for processing huge XML documents in tree mode")
    (description "@code{XML::Twig} is an XML transformation module.  Its
strong points: can be used to process huge documents while still being in tree
mode; not bound by DOM or SAX, so it is very perlish and offers a very
comprehensive set of methods; simple to use; DWIMs as much as possible.

What it doesn't offer: full SAX support (it can export SAX, but only reads
XML), full XPath support (unless you use @code{XML::Twig::XPath}), nor DOM
support.")
    (license license:perl-license)))
