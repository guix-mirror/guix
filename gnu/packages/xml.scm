;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages java)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public expat
  (package
    (name "expat")
    (version "2.2.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/expat/expat/"
                                 version "/expat-" version ".tar.bz2"))
             (sha256
              (base32
               "1wl1x93b5w457ddsdgj0lh7yjq4q6l7wfbgwhagkc8fm2qkkrd0p"))))
    (build-system gnu-build-system)
    (home-page "https://libexpat.github.io/")
    (synopsis "Stream-oriented XML parser library written in C")
    (description
     "Expat is an XML parser library written in C.  It is a
stream-oriented parser in which an application registers handlers for
things the parser might find in the XML document (like start tags).")
    (license license:expat)))

(define-public libebml
  (package
    (name "libebml")
    (version "1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.matroska.org/downloads/"
                           name "/" name "-" version ".tar.xz"))
       (sha256
        (base32
         "17iynkj22rbszaymxiaq6k02qy9w4fs1appkn1iam4y441w7lnhy"))
       (patches
        (list
         (origin
           (method url-fetch)
           (uri
            (string-append "https://github.com/Matroska-Org/libebml/commit/"
                           "e46906b80e7662fb78d305f576412f9fa4a22218.patch"))
           (file-name "libebml-use-limits-not-climits.patch")
           (sha256
            (base32
             "1803rcj4kzg385xij8j3fcz8h86z43ivciv512zr7jp9jwrafs58")))))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=YES")
       #:tests? #f))                    ; no test suite
    (home-page "https://matroska-org.github.io/libebml/")
    (synopsis "C++ library to parse EBML files")
    (description "libebml is a C++ library to read and write @dfn{EBML}
(Extensible Binary Meta Language) files.  EBML was designed to be a simplified
binary extension of XML for the purpose of storing and manipulating data in a
hierarchical form with variable field lengths.")
    (license license:lgpl2.1)))

(define-public libxml2
  (package
    (name "libxml2")
    (version "2.9.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://xmlsoft.org/libxml2/libxml2-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0ci7is75bwqqw2p32vxvrk6ds51ik7qgx73m920rakv5jlayax0b"))))
    (build-system gnu-build-system)
    (outputs '("out" "static"))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'move-static-libs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((src (string-append (assoc-ref outputs "out") "/lib"))
                            (dst (string-append (assoc-ref outputs "static")
                                                "/lib")))
                        (mkdir-p dst)
                        (for-each (lambda (ar)
                                    (rename-file ar (string-append dst "/"
                                                                   (basename ar))))
                                  (find-files src "\\.a$"))
                        #t))))))
    (home-page "http://www.xmlsoft.org/")
    (synopsis "C parser for XML")
    (inputs `(("xz" ,xz)))
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

(define-public python-libxml2
  (package/inherit libxml2
    (name "python-libxml2")
    (build-system python-build-system)
    (outputs '("out"))
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
                 (string-append glibc "/include"))))
            #t)))))
    (inputs `(("libxml2" ,libxml2)))
    (synopsis "Python bindings for the libxml2 library")))

(define-public python2-libxml2
  (package-with-python2 python-libxml2))

(define-public libxslt
  (package
    (name "libxslt")
    (version "1.1.32")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://xmlsoft.org/libxslt/libxslt-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0q2l6m56iv3ysxgm2walhg4c9wp7q183jb328687i9zlp85csvjj"))
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
    (home-page "https://metacpan.org/release/Graph-ReadWrite")
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
     ;; TODO package: perl-datetime-format-atom
     `(("perl-html-tagset" ,perl-html-tagset)
       ("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-class-data-inheritable" ,perl-class-data-inheritable)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-digest-sha1" ,perl-digest-sha1)
       ("perl-libwww" ,perl-libwww)
       ("perl-uri" ,perl-uri)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-xml-xpath" ,perl-xml-xpath)))
    (home-page "https://metacpan.org/release/XML-Atom")
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
    (home-page "https://metacpan.org/release/XML-Descent")
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
    (home-page "https://metacpan.org/release/XML-Parser")))

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
    (home-page "https://metacpan.org/release/XML-TokeParser")
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
    (synopsis "Perl modules for working with XML")
    (description
     "libxml-perl is a collection of smaller Perl modules, scripts, and
documents for working with XML in Perl.  libxml-perl software works in
combination with @code{XML::Parser}, PerlSAX, @code{XML::DOM},
@code{XML::Grove}, and others.")
    (home-page "https://metacpan.org/release/libxml-perl")))

(define-public perl-xml-libxml
  (package
    (name "perl-xml-libxml")
    (version "2.0134")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "XML-LibXML-" version ".tar.gz"))
       (sha256
        (base32
         "1ks69xymv6zkj7hvaymjvb78ch81abri7kg4zrwxhdfsqb8a9g7h"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-xml-namespacesupport" ,perl-xml-namespacesupport)
       ("perl-xml-sax" ,perl-xml-sax)))
    (inputs
     `(("libxml2" ,libxml2)))
    (home-page "https://metacpan.org/release/XML-LibXML")
    (synopsis "Perl interface to libxml2")
    (description "This module implements a Perl interface to the libxml2
library which provides interfaces for parsing and manipulating XML files.  This
module allows Perl programmers to make use of the highly capable validating
XML parser and the high performance DOM implementation.")
    (license license:perl-license)))

(define-public perl-xml-libxml-simple
  (package
    (name "perl-xml-libxml-simple")
    (version "0.99")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-LibXML-Simple-" version ".tar.gz"))
              (sha256
               (base32
                "0i4ybiqdnvnbfxqslw2y392kvy7i752dl8n99bqiqv5kzk4lbzhl"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-slurp-tiny" ,perl-file-slurp-tiny)
       ("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "https://metacpan.org/release/XML-LibXML-Simple")
    (synopsis "XML::LibXML based XML::Simple clone")
    (description
     "This package provides the same API as @code{XML::Simple} but is based on
@code{XML::LibXML}.")
    (license license:perl-license)))

(define-public perl-xml-libxslt
  (package
    (name "perl-xml-libxslt")
    (version "1.96")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                           "XML-LibXSLT-" version ".tar.gz"))
       (sha256
        (base32
         "0wyl8klgr65j8y8fzgwz9jlvfjwvxazna8j3dg9gksd2v973fpia"))))
    (build-system perl-build-system)
    (inputs
     `(("libxslt" ,libxslt)))
    (propagated-inputs
     `(("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "https://metacpan.org/release/XML-LibXSLT")
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
    (home-page "https://metacpan.org/release/XML-NamespaceSupport")
    (synopsis "XML namespace support class")
    (description "This module offers a simple to process namespaced XML
names (unames) from within any application that may need them.  It also helps
maintain a prefix to namespace URI map, and provides a number of basic
checks.")
    (license license:perl-license)))

(define-public perl-xml-rss
  (package
    (name "perl-xml-rss")
    (version "1.60")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/S/SH/SHLOMIF/"
                                  "XML-RSS-" version ".tar.gz"))
              (sha256
               (base32
                "0xw6aaqka3vqwbv152sbh6fbi8j306q1gvg7v83br8miif3mjcsb"))))
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
    (home-page "https://metacpan.org/release/XML-RSS")
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
    (version "1.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRANTM/"
                           "XML-SAX-" version ".tar.gz"))
       (sha256
        (base32
         "1qra9k3wszjxvsgbragl55z3qba4nri0ipmjaxfib4l6xxj6bsj5"))))
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
                                            "/lib/perl5/site_perl"))
                     #t)))))
    (home-page "https://metacpan.org/release/XML-SAX")
    (synopsis "Perl API for XML")
    (description "XML::SAX consists of several framework classes for using and
building Perl SAX2 XML parsers, filters, and drivers.")
    (license license:perl-license)))

(define-public perl-xml-sax-base
  (package
    (name "perl-xml-sax-base")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GR/GRANTM/"
                           "XML-SAX-Base-" version ".tar.gz"))
       (sha256
        (base32
         "1l1ai9g1z11ja7mvnfl5mj346r13jyckbg9qlw6c2izglidkbjv6"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/XML-SAX-Base")
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
    (version "2.25")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/G/GR/GRANTM/XML-Simple-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1y6vh328zrh085d40852v4ij2l4g0amxykswxd1nfhd2pspds7sk"))))
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
    (home-page "https://metacpan.org/release/XML-Simple")))

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
    (home-page "https://metacpan.org/release/XML-RegExp")))

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
    (home-page "https://metacpan.org/release/XML-DOM")))

(define-public perl-xml-compile-tester
  (package
    (name "perl-xml-compile-tester")
    (version "0.91")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-Tester-" version ".tar.gz"))
              (sha256
               (base32
                "1drzwziwi96rfkh48qpw4l225mcbk8ppl2157nj92cslcpwwdk75"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-test-deep" ,perl-test-deep)))
    (home-page "https://metacpan.org/release/XML-Compile-Tester")
    (synopsis "XML::Compile related regression testing")
    (description
     "The @code{XML::Compile} module suite has extensive regression testing.
This module provide functions which simplify writing tests for
@code{XML::Compile} related distributions.")
    (license license:perl-license)))

(define-public perl-xml-compile
  (package
    (name "perl-xml-compile")
    (version "1.61")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-" version ".tar.gz"))
              (sha256
               (base32
                "13dvsaj8simivj867rkjlf7hhvvcwlahnhk70zf8cq2xbg8wmf7x"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-carp" ,perl-carp)
       ("perl-log-report" ,perl-log-report)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-scalar-list-utils" ,perl-scalar-list-utils)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-types-serialiser" ,perl-types-serialiser)))
    (home-page "https://metacpan.org/release/XML-Compile")
    (synopsis "Compilation-based XML processing")
    (description
     "@code{XML::Compile} can be used to translate a Perl data-structure into
XML or XML into a Perl data-structure, both directions under rigid control by
a schema.")
    (license license:perl-license)))

(define-public perl-xml-compile-cache
  (package
    (name "perl-xml-compile-cache")
    (version "1.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-Cache-" version ".tar.gz"))
              (sha256
               (base32
                "181qf1s7ymgi7saph3cf9p6dbxkxyh1ja23na4dchhi8v5mi66sr"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)
       ("perl-xml-libxml-simple" ,perl-xml-libxml-simple)))
    (home-page "https://metacpan.org/release/XML-Compile-Cache")
    (synopsis "Cache compiled XML translators")
    (description
     "This package provides methods to cache compiled XML translators.")
    (license license:perl-license)))

(define-public perl-xml-compile-soap
  (package
    (name "perl-xml-compile-soap")
    (version "3.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-SOAP-" version ".tar.gz"))
              (sha256
               (base32
                "0pkcph562l2ij7rlwlvm58v6y062qsbydfpaz2qnph2ixqy0xfd1"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-file-slurp-tiny" ,perl-file-slurp-tiny)
       ("perl-libwww" ,perl-libwww)
       ("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-cache" ,perl-xml-compile-cache)
       ("perl-xml-compile-tester" ,perl-xml-compile-tester)))
    (home-page "https://metacpan.org/release/XML-Compile-SOAP")
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
    (version "3.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MARKOV/"
                                  "XML-Compile-WSDL11-" version ".tar.gz"))
              (sha256
               (base32
                "09ayl442hzvn97q4ghn5rz4r82dm9w3l69hixhb29h9xq9ysi7ba"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-log-report" ,perl-log-report)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-cache" ,perl-xml-compile-cache)
       ("perl-xml-compile-soap" ,perl-xml-compile-soap)))
    (home-page "https://metacpan.org/release/XML-Compile-WSDL11")
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
    (version "0.55")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/D/DA/DAVECROSS/"
                                  "XML-Feed-" version ".tar.gz"))
              (sha256
               (base32
                "0am345qzy5rxxnzh13l6p18a7drgkzmmlkgrgl4cv3b2j1pwls3i"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f))                    ; tests require internet connection
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
    (home-page "https://metacpan.org/release/XML-Feed")
    (synopsis "XML Syndication Feed Support")
    (description "@code{XML::Feed} is a syndication feed parser for both RSS and
Atom feeds.  It also implements feed auto-discovery for finding feeds, given a URI.
@code{XML::Feed} supports the following syndication feed formats:
RSS 0.91, RSS 1.0, RSS 2.0, Atom")
    (license license:perl-license)))

(define-public perl-xml-xpath
  (package
    (name "perl-xml-xpath")
    (version "1.44")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MA/MANWAR/"
                                  "XML-XPath-" version ".tar.gz"))
              (sha256
               (base32
                "03yxj7w5a43ibbpiqsvb3lswj2b71dydsx4rs2fw0p8n0l3i3j8w"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-path-tiny" ,perl-path-tiny)))
    (propagated-inputs
     `(("perl-xml-parser" ,perl-xml-parser)))
    (home-page "https://metacpan.org/release/XML-XPath")
    (synopsis "Parse and evaluate XPath statements")
    (description
     "This module aims to comply exactly to the @url{XPath specification,
https://www.w3.org/TR/xpath} and yet allow extensions to be added in
the form of functions.")
    (license license:perl-license)))

(define-public pugixml
  (package
    (name "pugixml")
    (version "1.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/zeux/pugixml/releases/download/v"
                          version "/pugixml-" version ".tar.gz"))
      (sha256
       (base32
        "19nv3zhik3djp4blc4vrjwrl8dfhzmal8b21sq7y907nhddx6mni"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DCMAKE_CXX_FLAGS=-shared -fPIC"
                           "-DCMAKE_C_FLAGS=-shared -fPIC")
       #:tests? #f))                    ; no tests
    (home-page "https://pugixml.org")
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
    (version "1.2.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.aleksey.com/xmlsec/download/"
                                  "xmlsec1-" version ".tar.gz"))
              (sha256
               (base32
                "1dlf263mvxj9n4lnhhjawc2hv45agrwjf8kxk7k8h9g9v2x5dmwp"))))
    (build-system gnu-build-system)
    (propagated-inputs                  ; according to xmlsec1.pc
     `(("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (inputs
     `(("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("libltdl" ,libltdl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.aleksey.com/xmlsec/")
    (synopsis "XML Security Library")
    (description
     "The XML Security Library is a C library based on Libxml2.  It
supports XML security standards such as XML Signature, XML Encryption,
Canonical XML (part of Libxml2) and Exclusive Canonical XML (part of
Libxml2).")
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public xmlsec-nss
  (package
    (inherit xmlsec)
    (name "xmlsec-nss")
    (native-inputs
     ;; For tests.
     `(("nss:bin" ,nss "bin")           ; for certutil
       ,@(package-native-inputs xmlsec)))
    (inputs
     `(("nss" ,nss)
       ("libltdl" ,libltdl)))
    (synopsis "XML Security Library (using NSS instead of GnuTLS)")))

(define-public minixml
  (package
    (name "minixml")
    (version "2.12")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/michaelrsweet/mxml/"
                                  "releases/download/v" version
                                  "/mxml-" version ".tar.gz"))
              (sha256
               (base32
                "1z8nqxa4pqdic8wpixkkgg1m2pak9wjikjjxnk3j5i0d29dbgmmg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-permissions
           ;; FIXME: url-fetch/tarbomb resets all permissions to 555/444.
           (lambda _
             (for-each
              (lambda (file)
                (chmod file #o644))
              (find-files "doc" "\\."))
             #t)))
       #:tests? #f))                    ; tests are run during build
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
              (file-name (string-append name "-" version ".tar.gz"))
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
             (invoke "g++" "-Wall" "-O2" "-shared" "-fpic"
                     "tinyxml.cpp" "tinyxmlerror.cpp"
                     "tinyxmlparser.cpp" "tinystr.cpp"
                     "-o" "libtinyxml.so")))
         (replace 'check
           (lambda _ (invoke "./xmltest")))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leethomason/tinyxml2.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a0skfi8rzk53qcxbv88qlvhlqzvsvg4hm20dnx4zw7vrn6anr9y"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests
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

(define-public html-xml-utils
 (package
   (name "html-xml-utils")
   (version "7.7")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://www.w3.org/Tools/HTML-XML-utils/html-xml-utils-"
            version ".tar.gz"))
      (sha256
       (base32
        "1vwqp5q276j8di9zql3kygf31z2frp2c59yjqlrvvwcvccvkcdwr"))))
   (build-system gnu-build-system)
   (home-page "https://www.w3.org/Tools/HTML-XML-utils/")
   (synopsis "Command line utilities to manipulate HTML and XML files")
   (description "HTML-XML-utils provides a number of simple utilities for
manipulating and converting HTML and XML files in various ways.  The suite
consists of the following tools:

@itemize
 @item @command{asc2xml} convert from @code{UTF-8} to @code{&#nnn;} entities
 @item @command{xml2asc} convert from @code{&#nnn;} entities to @code{UTF-8}
 @item @command{hxaddid} add IDs to selected elements
 @item @command{hxcite} replace bibliographic references by hyperlinks
 @item @command{hxcite} mkbib - expand references and create bibliography
 @item @command{hxclean} apply heuristics to correct an HTML file
 @item @command{hxcopy} copy an HTML file while preserving relative links
 @item @command{hxcount} count elements and attributes in HTML or XML files
 @item @command{hxextract} extract selected elements
 @item @command{hxincl} expand included HTML or XML files
 @item @command{hxindex} create an alphabetically sorted index
 @item @command{hxmkbib} create bibliography from a template
 @item @command{hxmultitoc} create a table of contents for a set of HTML files
 @item @command{hxname2id} move some @code{ID=} or @code{NAME=} from A
elements to their parents
 @item @command{hxnormalize} pretty-print an HTML file
 @item @command{hxnsxml} convert output of hxxmlns back to normal XML
 @item @command{hxnum} number section headings in an HTML file
 @item @command{hxpipe} convert XML to a format easier to parse with Perl or AWK
 @item @command{hxprintlinks} number links and add table of URLs at end of an HTML file
 @item @command{hxprune} remove marked elements from an HTML file
 @item @command{hxref} generate cross-references
 @item @command{hxselect} extract elements that match a (CSS) selector
 @item @command{hxtoc} insert a table of contents in an HTML file
 @item @command{hxuncdata} replace CDATA sections by character entities
 @item @command{hxunent} replace HTML predefined character entities to @code{UTF-8}
 @item @command{hxunpipe} convert output of pipe back to XML format
 @item @command{hxunxmlns} replace \"global names\" by XML Namespace prefixes
 @item @command{hxwls} list links in an HTML file
 @item @command{hxxmlns} replace XML Namespace prefixes by \"global names\"
@end itemize
")
   (license license:expat)))

(define-public xlsx2csv
  (package
    (name "xlsx2csv")
    (version "0.7.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dilshod/xlsx2csv.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "168dm6p7w6pvgd87yb9hcxv9y0liv6mxgril202nfva68cp8y939"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2               ; use python-2 for the test script
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (substitute* "test/run"
               ;; Run tests with `python' only.
               (("^(PYTHON_VERSIONS = ).*" all m) (string-append m "['']")))
             (invoke "test/run"))))))
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
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "defusedxml" version))
       (sha256
        (base32
         "1x54n0h8hl92vvwyymx883fbqpqjwn2mc8fb383bcg3z9zwz5mr4"))))
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
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libxls/libxls/releases/download/"
                           "v" version "/libxls-" version ".tar.gz"))
       (sha256
        (base32 "00j2lrcvvhclmh3z9vy7myqq1br1jnnqkz2wzgk4a1gbg8c5afn5"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/libxls/libxls")
    (synopsis "Read binary (.xls) Excel spreadsheet files")
    (description
     "libxls is a C library to read .xls spreadsheet files in the binary OLE
BIFF8 format as created by Excel 97 and later versions.  It cannot write them.

This package also provides @command{xls2csv} to export Excel files to CSV.")
    (license license:bsd-2)))

(define-public freexl
  (package
    (name "freexl")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.gaia-gis.it/gaia-sins/"
                                  name  "-" version ".tar.gz"))
              (sha256
               (base32
                "03bmwq6hngmzwpqpb7c2amqlspz4q69iv96nlf0f5c0qs98b3j9x"))))
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
                "0w19k1awslmihpwsxwjbg89hv0vjhk4k3i0vrfchy3mqknd988y5"))
              (patches (search-patches "java-simple-xml-fix-tests.patch"))))
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
    (home-page "https://metacpan.org/release/XML-XPathEngine")
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
    (home-page "https://metacpan.org/release/Tree-XPathEngine")
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
    (home-page "https://metacpan.org/release/XML-Filter-BufferText")
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
       ("perl-xml-namespacesupport" ,perl-xml-namespacesupport)
       ("perl-xml-sax-base" ,perl-xml-sax-base)))
    (home-page "https://metacpan.org/release/XML-SAX-Writer")
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
    (home-page "https://metacpan.org/release/XML-Handler-YAWriter")
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
       ("perl-test-pod" ,perl-test-pod)
       ("perl-tree-xpathengine" ,perl-tree-xpathengine)))
    (home-page "https://metacpan.org/release/XML-Twig")
    (synopsis "Perl module for processing huge XML documents in tree mode")
    (description "@code{XML::Twig} is an XML transformation module.  Its
strong points: can be used to process huge documents while still being in tree
mode; not bound by DOM or SAX, so it is very perlish and offers a very
comprehensive set of methods; simple to use; DWIMs as much as possible.

What it doesn't offer: full SAX support (it can export SAX, but only reads
XML), full XPath support (unless you use @code{XML::Twig::XPath}), nor DOM
support.")
    (license license:perl-license)))

;; TODO: Debian builds several jars out of this: jaxp-1.4.jar,
;; xml-apis.jar and xml-apis-1.4.01.jar.
(define-public java-jaxp
  (package
    (name "java-jaxp")
    (version "1.4.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/xml-commons/source/"
                           "xml-commons-external-" version "-src.tar.gz"))
       (sha256
        (base32 "0rhq32a7dl9yik7zx9h0naz2iz068qgcdiayak91wp4wr26xhjyk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxp.jar"
       #:jdk ,icedtea-8
       #:source-dir ".."
       #:tests? #f)); no tests
    (home-page "http://xerces.apache.org/xml-commons/")
    (synopsis "Java XML parser and transformer APIs (DOM, SAX, JAXP, TrAX)")
    (description "Jaxp from the Apache XML Commons project is used by
the Xerces-J XML parser and Xalan-J XSLT processor and specifies these APIs:

@itemize
@item Document Object Model (DOM)
@item Simple API for XML (SAX)
@item Java APIs for XML Processing (JAXP)
@item Transformation API for XML (TrAX)
@item Document Object Model (DOM) Load and Save
@item JSR 206 Java API for XML Processing
@end itemize")
    (license (list license:asl2.0
                   license:w3c ;; Files under org.w3c
                   license:public-domain)))) ;; org.xml.sax

(define-public java-apache-xml-commons-resolver
  (package
    (name "java-apache-xml-commons-resolver")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/xml-commons/"
                           "xml-commons-resolver-" version ".tar.gz"))
       (sha256
        (base32 "1zhy4anc3fg9f8y348bj88vmab15aavrg6nf419ifb25asyygnsm"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file (find-files "." ".*\\.(jar|zip)"))
           #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append "xml-resolver.jar")
       #:tests? #f)); no tests
    (inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://xerces.apache.org/xml-commons/")
    (synopsis "Catalog-based entity and URI resolution")
    (description "The resolver class implements the full semantics of OASIS Technical
Resolution 9401:1997 (Amendment 2 to TR 9401) catalogs and the 06 Aug
2001 Committee Specification of OASIS XML Catalogs.

It also includes a framework of classes designed to read catalog files
in a number of formats:

@itemize
@item The plain-text flavor described by TR9401.
@item The XCatalog XML format defined by John Cowan
@item The XML Catalog format defined by the OASIS Entity Resolution
      Technical Committee.
@end itemize")
    (license license:asl2.0)))

;; Jaxen requires java-dom4j and java-xom that in turn require jaxen.
;; This package is a bootstrap version without dependencies on dom4j and xom.
(define java-jaxen-bootstrap
  (package
    (name "java-jaxen-bootstrap")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              ;; No release on github
              (uri (string-append "https://repo1.maven.org/maven2/jaxen/jaxen/"
                                  version "/jaxen-" version "-sources.jar"))
              (sha256
               (base32
                "18pa8mks3gfhazmkyil8wsp6j1g1x7rggqxfv4k2mnixkrj5x1kx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxen.jar"
       #:source-dir "src"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-dom4j
           (lambda _
             (delete-file-recursively "src/org/jaxen/dom4j")
             (delete-file-recursively "src/org/jaxen/xom")
             #t)))))
    (inputs
     `(("java-jdom" ,java-jdom)))
    (home-page "https://github.com/jaxen-xpath/jaxen")
    (synopsis "XPath library")
    (description "Jaxen is an XPath library written in Java.  It is adaptable
to many different object models, including DOM, XOM, dom4j, and JDOM.  It is
also possible to write adapters that treat non-XML trees such as compiled
Java byte code or Java beans as XML, thus enabling you to query these trees
with XPath too.")
    (license license:bsd-3)))

(define-public java-jaxen
  (package
    (inherit java-jaxen-bootstrap)
    (name "java-jaxen")
    (inputs
     `(("java-jdom" ,java-jdom)
       ("java-xom" ,java-xom)
       ("java-dom4j" ,java-dom4j)))))

(define-public java-xom
  (package
    (name "java-xom")
    (version "127")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elharo/xom.git")
                    (commit (string-append "XOM_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jh6y03g5zzdhsb5jm6ms1xnamr460qmn96y3w6aw0ikfwqlg0bq"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xom.jar"
       #:jdk ,icedtea-8
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'configure 'fix-tagsoup-dep
           (lambda _
             ;; FIXME: Where is tagsoup source?
             (delete-file "src/nu/xom/tools/XHTMLJavaDoc.java")
             #t)))))
    (inputs
     `(("java-jdom" ,java-jdom)
       ("java-junit" ,java-junit)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-jaxen-bootstrap" ,java-jaxen-bootstrap)
       ("java-xerces" ,java-xerces)))
    (home-page "https://xom.nu/")
    (synopsis "XML Object Model")
    (description "XOM is a new XML Object Model for processing XML with Java 
that strives for correctness and simplicity.")
    ;; 2.1 only
    (license license:lgpl2.1)))

(define-public java-xsdlib
  (package
    (name "java-xsdlib")
    (version "2013.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/com/sun/msv/"
                                  "datatype/xsd/xsdlib/" version "/xsdlib-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "185i48p1xp09wbq03i9zgfl701qa262rq46yf4cajzmk3336kqim"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:jar-name "xsdlib.jar"
       #:jdk ,icedtea-8))
    (inputs
     `(("java-xerces" ,java-xerces)))
    (home-page "http://central.maven.org/maven2/com/sun/msv/datatype/xsd/xsdlib/")
    (synopsis "Sun Multi-Schema Validator")
    (description "Xsdlib contains an implementation of sun.com.msv, an XML
validator.")
    (license license:bsd-2)))

(define-public java-xpp3
  (package
    (name "java-xpp3")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.extreme.indiana.edu/dist/"
                                  "java-repository/xpp3/distributions/xpp3-"
                                  version "_src.tgz"))
              (sha256
               (base32
                "1b99zrhyij5qwyhilyjdl1ykxvhk902vsvflh6gx4fir8hfvdl5p"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-target "jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "build")))))
    (home-page "http://www.extreme.indiana.edu/xgws/xsoap/xpp/")
    (synopsis "Streaming pull XML parser")
    (description "Xml Pull Parser (in short XPP) is a streaming pull XML
parser and should be used when there is a need to process quickly and
efficiently all input elements (for example in SOAP processors). This
package is a stable XmlPull parsing engine that is based on ideas from XPP
and in particular XPP2 but completely revised and rewritten to take the best
advantage of JIT JVMs.")
    (license (license:non-copyleft "file://LICENSE.txt"))))

(define-public java-xmlpull2
  (package
    (name "java-xmlpull2")
    (version "2.1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.extreme.indiana.edu/xgws/xsoap/"
                                  "PullParser/PullParser" version ".tgz"))
              (sha256
               (base32
                "1kw9nhyqb7bzhn2zjbwlpi5vp5rzj89amzi3hadw2acyh2dmd0md"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-target "impl"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars "build/lib")))))
    (home-page "http://www.extreme.indiana.edu/xgws/xsoap/xpp/")
    (synopsis "Streaming pull XML parser")
    (description "Xml Pull Parser (in short XPP) is a streaming pull XML
parser and should be used when there is a need to process quickly and
efficiently all input elements (for example in SOAP processors).  This
package is in maintenance mode.")
    (license (license:non-copyleft "file:///LICENSE.txt"))))

(define-public java-dom4j
  (package
    (name "java-dom4j")
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dom4j/dom4j.git")
                    (commit (string-append "version-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1827jljs8mps489fm7xw63cakdqwc5grilrr5n9spr2rlk76jpx3"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "dom4j.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       ;; FIXME: Requires xalan, but xalan depends on java-cup which has a
       ;; dependency on itself through jflex.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-jaxen-sources
           ;; java-jaxen-bootstrap is not enough. These files have a circular
           ;; dependency and there is no subset of dom4j that would allow
           ;; breaking the circle.
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "jaxen-sources")
             (with-directory-excursion "jaxen-sources"
               (system* "jar" "xf" (assoc-ref inputs "java-jaxen-sources")))
             (mkdir-p "src/main/java/org/jaxen/dom4j")
             (copy-file "jaxen-sources/org/jaxen/dom4j/DocumentNavigator.java"
                        "src/main/java/org/jaxen/dom4j/DocumentNavigator.java")
             (copy-file "jaxen-sources/org/jaxen/dom4j/Dom4jXPath.java"
                        "src/main/java/org/jaxen/dom4j/Dom4jXPath.java")
             #t))
         (add-before 'build 'fix-old-xpp2
           (lambda _
             ;; This package normally depends on xpp2 2.0, but version 2.1.10
             ;; is the only version whose source code is published.
             (substitute* "src/main/java/org/dom4j/xpp/ProxyXmlStartTag.java"
               (("public void resetStartTag")
                "public boolean removeAttributeByRawName(String name) {\n
  return false;\n
}\n
public boolean removeAttributeByName(String name, String name2) {\n
  return false;\n
}\n\npublic void resetStartTag")
               (("Atttribute") "Attribute"))
             #t)))))
    (inputs
     `(("java-jaxen-bootstrap" ,java-jaxen-bootstrap)
       ("java-jaxen-sources" ,(package-source java-jaxen-bootstrap))
       ("java-xmlpull2" ,java-xmlpull2)
       ("java-xpp3" ,java-xpp3)
       ("java-xsdlib" ,java-xsdlib)))
    (native-inputs
     `(("java-testng" ,java-testng)
       ("java-xerces" ,java-xerces)))
    (home-page "https://dom4j.github.io/")
    (synopsis "Flexible XML framework for Java")
    (description "Dom4j is a flexible XML framework for Java.  DOM4J works
with DOM, SAX, XPath, and XSLT.  It can parse large XML documents with very
low memory footprint.")
    ;; some BSD-like 5-clause license
    (license (license:non-copyleft "file://LICENSE"))))

(define-public java-kxml2
  (package
    (name "java-kxml2")
    (version "2.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stefanhaustein/kxml2.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g6d8c9r9sh3x04sf4wdpgwvhkqvk11k3kq9skx91i60h4vn01hg"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "kxml2.jar"
       #:source-dir "src/main/java"
       #:test-include (list "TestWb.java")
       ;; Test failure: it was expected to get an XML entity but got the
       ;; equivalent Unicode character instead.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-xpp3" ,java-xpp3)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://kxml.org")
    (synopsis "XML pull parser")
    (description "kXML is a small XML pull parser, specially designed for
constrained environments such as Applets, Personal Java or devices compliant
with the Mobile Information Device Profile (MIDP).")
    (license license:expat)))

(define-public java-stax
  (package
    (name "java-stax")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/stax/stax/"
                                  version "/stax-" version "-sources.jar"))
              (sha256
               (base32
                "04ba4qvbrps45j8bldbakxq31k7gjlsay9pppa9yn13fr00q586z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "stax.jar"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-utf8
           (lambda _
             ;; This file is ISO-8859-1 but java expects UTF-8.
             ;; Remove special characters in comments.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "src/com/wutka/dtd/Scanner.java"
                 (("//.*") "\n")))
             #t)))))
    (home-page "https://repo1.maven.org/maven2/stax/stax/")
    (synopsis "Streaming API for XML")
    (description "This package provides the reference implementation of the
@dfn{Streaming API for XML} (StAX).  It is used for streaming XML data to
and from a Java application.  It provides a standard pull parser interface.")
    (license license:asl2.0)))

(define-public java-jettison
  (package
    (name "java-jettison")
    (version "1.3.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/codehaus/jettison.git")
                    (commit (string-append "jettison-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15sydmi5chdh4126qc7v8bsrp7fp4ldaya8a05iby4pq2324q0qw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jettison.jar"
       #:source-dir "src/main/java"
       #:test-exclude (list "**/Abstract*.java"
                            ;; Abstract classes
                            "**/DOMTest.java"
                            "**/BadgerFishDOMTest.java"
                            "**/MappedDOMTest.java")))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://github.com/codehaus/jettison")
    (synopsis "StAX implementation for JSON")
    (description "Jettison is a Java library for converting XML to JSON and
vice-versa with the help of the @dfn{Streaming API for XML} (StAX).  It
implements @code{XMLStreamWriter} and @code{XMLStreamReader} and supports
@code{Mapped} and @code{BadgerFish} conventions.")
    (license license:asl2.0)))

(define-public java-jdom2
  (package
    (name "java-jdom")
    (version "2.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hunterhacker/jdom.git")
                    (commit (string-append "JDOM-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14vv1kxrsdvwi4cz3rx6r48w5y6fvk9cymil8qhvxwp56xxrgxiq"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "package"
       #:tests? #f                ; tests are run as part of the build process
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "build")))))
    (home-page "http://jdom.org/")
    (synopsis "Access, manipulate, and output XML data")
    (description "Jdom is a Java-based solution for accessing, manipulating, and
outputting XML data from Java code.")
    (license license:bsd-4)))

(define-public java-xstream
  (package
    (name "java-xstream")
    (version "1.4.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/x-stream/xstream.git")
             (commit (string-append
                      "XSTREAM_"
                      (string-map (lambda (x) (if (eq? x #\.) #\_ x))
                                  version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12m2bw8bapdc1w0pni9wl5hh2y8jfdgcvxd464jl9917dsp3ai2n"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xstream.jar"
       ;; FIXME: Tests are not in a java subdirectory as assumed by ant-build-system.
       #:tests? #f
       #:jdk ,icedtea-8
       #:source-dir "xstream/src/java"))
    (inputs
     `(("java-jdom" ,java-jdom)
       ("java-jdom2" ,java-jdom2)
       ("java-cglib" ,java-cglib)
       ("java-joda-time" ,java-joda-time)
       ("java-jettison" ,java-jettison)
       ("java-xom" ,java-xom)
       ("java-xpp3" ,java-xpp3)
       ("java-dom4j" ,java-dom4j)
       ("java-stax2-api" ,java-stax2-api)
       ("java-woodstox-core" ,java-woodstox-core)
       ("java-kxml2" ,java-kxml2)
       ("java-stax" ,java-stax)))
    (home-page "https://x-stream.github.io")
    (synopsis "XML serialization library")
    (description "XStream is a simple library to serialize Java objects to XML
and back again.")
    (license license:bsd-3)))

(define-public ghc-hxt-charproperties
  (package
    (name "ghc-hxt-charproperties")
    (version "9.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hxt-charproperties/hxt-charproperties-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1mml8wglvagqq891rchgli6r8rnkwrqhgsxfl6kb5403pzb18rp4"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/UweSchmidt/hxt")
    (synopsis "Character properties and classes for XML and Unicode")
    (description
     "The modules provided by this package contain predicates for Unicode
blocks and char properties and character predicates defined by XML.  The
supported Unicode version is 7.0.0")
    (license license:expat)))

(define-public ghc-hxt-unicode
  (package
    (name "ghc-hxt-unicode")
    (version "9.0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hxt-unicode/hxt-unicode-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0rj48cy8z4fl3zpg5bpa458kqr83adav6jnqv4i71dclpprj6n3v"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hxt-charproperties" ,ghc-hxt-charproperties)))
    (home-page
     "http://www.fh-wedel.de/~si/HXmlToolbox/index.html https://github.com/UweSchmidt/hxt")
    (synopsis
     "Unicode en-/decoding functions for utf8, iso-latin-* and other encodings")
    (description
     "This package provides Unicode encoding and decoding functions for
encodings used in the Haskell XML Toolbox.  ISO Latin 1-16, utf8, utf16, ASCII
are supported. Decoding is done with lazy functions, errors may be detected or
ignored.")
    (license license:expat)))

(define-public ghc-hxt-regex-xmlschema
  (package
    (name "ghc-hxt-regex-xmlschema")
    (version "9.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hxt-regex-xmlschema/hxt-regex-xmlschema-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1c4jr0439f5yc05h7iz53fa47g6l2wrvqp6gvwf01mlqajk3nx7l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hxt-charproperties" ,ghc-hxt-charproperties)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-text" ,ghc-text)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "http://www.haskell.org/haskellwiki/Regular_expressions_for_XML_Schema")
    (synopsis "Regular expression library for W3C XML Schema regular expressions")
    (description
     "This library supports full W3C XML Schema regular expressions inclusive
all Unicode character sets and blocks.  It is implemented by the technique of
derivations of regular expressions.")
    (license license:expat)))

(define-public ghc-hxt
  (package
    (name "ghc-hxt")
    (version "9.3.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hxt/hxt-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qq3ykgn355rx242xjcbqqksgvwr6k2fdj5phw4iv28qqxff6m8d"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-parsec" ,ghc-parsec)
       ("ghc-hxt-charproperties" ,ghc-hxt-charproperties)
       ("ghc-hxt-unicode" ,ghc-hxt-unicode)
       ("ghc-hxt-regex-xmlschema" ,ghc-hxt-regex-xmlschema)
       ("ghc-network-uri" ,ghc-network-uri)))
    (home-page "https://github.com/UweSchmidt/hxt")
    (synopsis "Collection of tools for processing XML with Haskell")
    (description
     "The Haskell XML Toolbox bases on the ideas of HaXml and HXML, but
introduces a more general approach for processing XML with Haskell.")
    (license license:expat)))

(define-public xmlrpc-c
  (package
    (name "xmlrpc-c")
    (version "1.43.08")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/xmlrpc-c/Xmlrpc-c%20Super%20Stable/"
                                 version "/xmlrpc-c-" version ".tgz"))
             (sha256
              (base32
               "18zwbj6i2hpcn5riiyp8i6rml0sfv60dd7phw1x8g4r4lj2bbxf9"))))
    (build-system gnu-build-system)
    (inputs
     `(("curl" ,curl)))
    (native-inputs
     `(;; For tools, if ever needed.
       ("perl" ,perl)))
    (arguments
     `(#:make-flags ; Add $libdir to the RUNPATH of all the executables.
       (list (string-append "LDFLAGS_PERSONAL=-Wl,-rpath=" %output "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-/bin/sh-in-tests
           (lambda _
             (substitute* "GNUmakefile"
               (("#! /bin/sh") (which "sh")))
             #t)))))
    (home-page "http://xmlrpc-c.sourceforge.net/")
    (synopsis "Lightweight RPC library based on XML and HTTP")
    (description
     "XML-RPC is a quick-and-easy way to make procedure calls over the Internet.
It converts the procedure call into an XML document, sends it to a remote
server using HTTP, and gets back the response as XML.  This library provides a
modular implementation of XML-RPC for C and C++.")
    (license (list license:psfl license:expat))))

(define-public python-xmltodict
  (package
    (name "python-xmltodict")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xmltodict" version))
        (sha256
          (base32
            "1pxh4yjhvmxi1h6f92skv41g4kbsws3ams57150kzn18m907v3cg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/martinblech/xmltodict")
    (synopsis "Work with XML like you are working with JSON")
    (description "This package provides a Python library to convert XML to
@code{OrderedDict}.")
    (license license:expat)))
