;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages documentation)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml))

(define-public asciidoc
  (package
    (name "asciidoc")
    (version "8.6.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/asciidoc/asciidoc/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10xrl1iwyvs8aqm0vzkvs3dnsn93wyk942kk4ppyl6w9imbzhlly"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                     ; no 'check' target
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "autoconf")))
         ;; Some XML-related binaries are required for asciidoc's proper usage.
         ;; Without these, asciidoc fails when parsing XML documents, either
         ;; reporting a missing "xmllint" binary or, when passed the
         ;; "--no-xmllint" option, a missing "xsltproc" binary.
         ;; The following phase enables asciidoc to find some of them.
         (add-before 'configure 'set-xml-binary-paths
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let* ((libxml2 (assoc-ref inputs "libxml2"))
                              (xmllint (string-append libxml2 "/bin/xmllint"))
                              (libxslt (assoc-ref inputs "libxslt"))
                              (xsltproc (string-append libxslt "/bin/xsltproc")))
                         (substitute* "a2x.py"
                           (("XMLLINT = 'xmllint'")
                            (string-append "XMLLINT = '" xmllint "'"))
                           (("XSLTPROC = 'xsltproc'")
                            (string-append "XSLTPROC = '" xsltproc "'")))
                         #t)))
         ;; Make asciidoc use the local docbook-xsl package instead of fetching
         ;; it from the internet at run-time.
         (add-before 'install 'make-local-docbook-xsl
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* (find-files "docbook-xsl" ".*\\.xsl$")
                         (("xsl:import href=\"http://docbook.sourceforge.net/\
release/xsl/current")
                          (string-append
                           "xsl:import href=\""
                           (string-append (assoc-ref inputs "docbook-xsl")
                                          "/xml/xsl/docbook-xsl-"
                                          ,(package-version docbook-xsl)))))
                       #t))
         ;; Do the same for docbook-xml.
         (add-before 'install 'make-local-docbook-xml
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "docbook45.conf"
                         (("http://www.oasis-open.org/docbook/xml/4.5/docbookx.dtd")
                          (string-append (assoc-ref inputs "docbook-xml")
                                         "/xml/dtd/docbook/docbookx.dtd")))
                       #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)))
    (inputs `(("python" ,python-2)
              ("docbook-xml" ,docbook-xml)
              ("docbook-xsl" ,docbook-xsl)
              ("libxml2" ,libxml2)
              ("libxslt" ,libxslt)))
    (home-page "http://asciidoc.org/")
    (synopsis "Text-based document generation system")
    (description
     "AsciiDoc is a text document format for writing notes, documentation,
articles, books, ebooks, slideshows, web pages, man pages and blogs.
AsciiDoc files can be translated to many formats including HTML, PDF,
EPUB, man page.

AsciiDoc is highly configurable: both the AsciiDoc source file syntax and
the backend output markups (which can be almost any type of SGML/XML
markup) can be customized and extended by the user.")
    (license gpl2+)))

(define-public doxygen
  (package
    (name "doxygen")
    (version "1.8.14")
    (home-page "http://www.doxygen.nl/")
    (source (origin
             (method url-fetch)
             (uri (list (string-append home-page "files/doxygen-"
                                       version ".src.tar.gz")
                        (string-append "mirror://sourceforge/doxygen/rel-"
                                       version "/doxygen-" version
                                       ".src.tar.gz")))
             (sha256
              (base32
               "0kcxymbam9jwiyjwyvwdjj0h74lbb6c467szsipzbxjyfl17wxfi"))
             (patches (search-patches "doxygen-test.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex-2.6.1) ; sefaults with 2.6.4
       ("libxml2" ,libxml2) ; provides xmllint for the tests
       ("python" ,python-2))) ; for creating the documentation
    (inputs
     `(("bash" ,bash-minimal)))
    (arguments
     `(#:test-target "tests"
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'patch-sh
                              (lambda* (#:key inputs #:allow-other-keys)
                                (substitute* "src/portable.cpp"
                                  (("/bin/sh")
                                   (string-append
                                    (assoc-ref inputs "bash") "/bin/sh")))
                                #t)))))
    (synopsis "Generate documentation from annotated sources")
    (description "Doxygen is the de facto standard tool for generating
documentation from annotated C++ sources, but it also supports other popular
programming languages such as C, Objective-C, C#, PHP, Java, Python,
IDL (Corba, Microsoft, and UNO/OpenOffice flavors), Fortran, VHDL, Tcl,
and to some extent D.")
    (license gpl3+)))

(define-public doc++
  (package
    (name "doc++")
    (version "3.4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceforge.net/projects/docpp/"
                                  "files/doc++-" version ".tar.gz"))
              (sha256
               (base32
                "0i37zlxl8g352s4hzpdx0657k5x3czh3xcsfr27irc708gb277pn"))
              (patches (search-patches "doc++-include-directives.patch"
                                       "doc++-segfault-fix.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)
       ("gettext" ,gettext-minimal)))
    (home-page "http://docpp.sourceforge.net/")
    (synopsis "Documentation system for C, C++, IDL, and Java")
    (description
     "DOC++ is a documentation system for C, C++, IDL, and Java.  It can
generate both TeX output for high-quality hardcopies or HTML output for online
brwosing.  The documentation is extracted directly from the C/C++/IDL source
or Java class files.")
    (license gpl2+)))

(define-public scrollkeeper
  (package
    (name "scrollkeeper")
    (version "0.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/scrollkeeper/scrollkeeper/"
                           version "/scrollkeeper-" version ".tar.gz"))
       (sha256
        (base32 "1bfxwxc1ngh11v36z899sz9qam366r050fhkyb5adv65lb1x62sa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xml-catalog="
                            (assoc-ref %build-inputs "docbook-xml")
                            "/xml/dtd/docbook/catalog.xml"))))
    (inputs
     `(("perl" ,perl)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ;; The configure script checks for either version 4.2 or 4.1.2.
       ("docbook-xml" ,docbook-xml-4.2)))
    (native-inputs
     `(("intltool" ,intltool)))
    (home-page "http://scrollkeeper.sourceforge.net/")
    (synopsis "Open Documentation Cataloging Project")
    (description "ScrollKeeper is a cataloging system for documentation on open
systems.  It manages documentation metadata as specified by the Open Source
Metadata Framework and provides a simple API to allow help browsers to find,
sort, and search the document catalog.  It will also be able to communicate
with catalog servers on the Net to search for documents which are not on the
local system.")
    (license lgpl2.1+)))
