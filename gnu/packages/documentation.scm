;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020, 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix deprecation)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public latex2html
  (package
    (name "latex2html")
    (version "2020.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/latex2html/latex2html")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1icyl6kl60wh7cavprgbd8q6lpjwr7wn24m34kpiif7ahknhcbcm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure"
               (("/usr/local")
                (assoc-ref outputs "out"))
               (("\\$\\{CONFIG_SHELL-/bin/sh\\}")
                (which "bash")))
             #t))
         (replace 'configure
           (lambda _
             (invoke "./configure")
             #t))
         (add-after 'configure 'patch-cfgcache
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "cfgcache.pm"
               (("/usr/local")
                (assoc-ref outputs "out")))
             #t)))))
    (inputs
     (list perl))
    (synopsis "LaTeX documents to HTML")
    (description "LaTeX2HTML is a utility that converts LaTeX documents to web
pages in HTML.")
    (home-page "https://www.latex2html.org/")
    (license gpl2+)))

(define-public asciidoc
  (package
    (name "asciidoc")
    (version "9.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/asciidoc/asciidoc-py")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1clf1axkns23wfmh48xfspzsnw04pjh4mq1pshpzvj0cwxhz0yaq"))))
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
     (list autoconf))
    (inputs
     (list python docbook-xml docbook-xsl libxml2 libxslt))
    (home-page "https://asciidoc.org/")
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

(define-deprecated asciidoc-py3 asciidoc)

(define-public doxygen
  (package
    (name "doxygen")
    (version "1.9.1")
    (home-page "https://www.doxygen.nl/")
    (source (origin
             (method url-fetch)
             (uri (list (string-append home-page "files/doxygen-"
                                       version ".src.tar.gz")
                        (string-append "mirror://sourceforge/doxygen/rel-"
                                       version "/doxygen-" version
                                       ".src.tar.gz")))
             (sha256
              (base32
               "1lcif1qi20gf04qyjrx7x367669g17vz2ilgi4cmamp1whdsxbk7"))))
    (build-system cmake-build-system)
    (native-inputs
     (list bison flex libxml2 ;provides xmllint for the tests
           python))             ;for creating the documentation
    (inputs
     `(("bash" ,bash-minimal)))
    (arguments
     ;; Force cmake to use iconv header from cross-libc instead of the one
     ;; from native libc.
     `(,@(if (%current-target-system)
             '(#:configure-flags
               (list (string-append "-DICONV_INCLUDE_DIR="
                                    (assoc-ref %build-inputs "cross-libc")
                                    "/include")))
             '())
       #:test-target "tests"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-bibtex-test
                    (lambda _
                      ;; Disable test that requires bibtex to avoid a
                      ;; circular dependency.
                      (for-each delete-file-recursively
                                '("testing/012" "testing/012_cite.dox"))))
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
     (list flex gettext-minimal))
    (home-page "http://docpp.sourceforge.net/")
    (synopsis "Documentation system for C, C++, IDL, and Java")
    (description
     "DOC++ is a documentation system for C, C++, IDL, and Java.  It can
generate both TeX output for high-quality hardcopies or HTML output for online
browsing.  The documentation is extracted directly from the C/C++/IDL source
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
     (list perl libxml2 libxslt
           ;; The configure script checks for either version 4.2 or 4.1.2.
           docbook-xml-4.2))
    (native-inputs
     (list intltool))
    (home-page "http://scrollkeeper.sourceforge.net/")
    (synopsis "Open Documentation Cataloging Project")
    (description "ScrollKeeper is a cataloging system for documentation on open
systems.  It manages documentation metadata as specified by the Open Source
Metadata Framework and provides a simple API to allow help browsers to find,
sort, and search the document catalog.  It will also be able to communicate
with catalog servers on the Net to search for documents which are not on the
local system.")
    (license lgpl2.1+)))

(define-public zeal
  (let ((commit "d3c5521c501d24050f578348ff1b9d68244b992c")
        (revision "1"))
    (package
      (name "zeal")
      (version (git-version "0.6.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zealdocs/zeal")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ky2qi2cmjckc51lm3i28815ixgqdm36j7smixxr16jxpmbqs6sl"))))
      (build-system qt-build-system)
      (arguments
       `(#:tests? #f                    ;no tests
         #:phases
         (modify-phases %standard-phases
           (add-after 'wrap 'wrap-qt-process-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin/zeal"))
                      (qt-process-path (string-append
                                        (assoc-ref inputs "qtwebengine")
                                        "/lib/qt5/libexec/QtWebEngineProcess")))
                 (wrap-program bin
                   `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))
                 #t))))))
      (native-inputs
       (list extra-cmake-modules pkg-config))
      (inputs
       `(("libarchive" ,libarchive)
         ("sqlite" ,sqlite)
         ("qtbase" ,qtbase-5)
         ("qtdeclarative" ,qtdeclarative)
         ("qtwebchannel" ,qtwebchannel)
         ("qtwebengine" ,qtwebengine)
         ("qtquickcontrols" ,qtquickcontrols)
         ("qtx11extras" ,qtx11extras)
         ("xcb-util-keyms" ,xcb-util-keysyms)))
      (home-page "https://zealdocs.org/")
      (synopsis "Offline documentation browser inspired by Dash")
      (description "Zeal is a simple offline documentation browser
inspired by Dash.")
      (license gpl3+))))
