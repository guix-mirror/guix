;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
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

(define-module (gnu packages libreoffice)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (lgpl2.1+ mpl2.0))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages doxygen)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define-public ixion
  (package
    (name "ixion")
    (version "0.9.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://kohei.us/files/ixion/src/libixion-"
                          version ".tar.xz"))
      (sha256 (base32
               "18g3nk29ljiqbyi0ml49j2x3f3xrqckdm9i66sw5fxnj7hb5rqvp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
     (inputs
      `(("mdds" ,mdds)
        ("python" ,python-2))) ; looks for python.pc, not python3.pc
    (home-page "https://gitlab.com/ixion/ixion")
    (synopsis "General purpose formula parser and interpreter")
    (description "Ixion is a library for calculating the results of formula
expressions stored in multiple named targets, or \"cells\".  The cells can
be referenced from each other, and the library takes care of resolving
their dependencies automatically upon calculation.")
    (license mpl2.0)))

(define-public orcus
  (package
    (name "orcus")
    (version "0.7.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://kohei.us/files/" name "/src/lib"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "0hva4qalg3dk6n1118ncr5fy8cqnj2f7fwldw7aa04124rj6p104"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
     (inputs
      `(("ixion" ,ixion)
        ("mdds" ,mdds)
        ("zlib" ,zlib)))
    (home-page "https://gitlab.com/orcus/orcus")
    (synopsis "File import filter library for spreadsheet documents")
    (description "Orcus is a library that provides a collection of standalone
file processing filters.  It is currently focused on providing filters for
spreadsheet documents.  The library includes import filters for
Microsoft Excel 2007 XML, Microsoft Excel 2003 XML, Open Document Spreadsheet,
Plain Text, Gnumeric XML, Generic XML.  It also includes low-level parsers for
CSV, CSS and XML.")
    (license mpl2.0)))

(define-public librevenge
  (package
    (name "librevenge")
    (version "0.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpd/" name "/" name "-"
                          version ".tar.xz"))
      (sha256 (base32
               "03ygxyb0vfjv8raif5q62sl33b54wkr5rzgadb8slijm6k281wpn"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("boost" ,boost)
       ("cppunit" ,cppunit)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("zlib" ,zlib)))
    (arguments
     ;; avoid triggering configure errors by simple inclusion of boost headers
     `(#:configure-flags '("--disable-werror")))
    (home-page "http://sourceforge.net/p/libwpd/wiki/librevenge/")
    (synopsis "Document importer for office suites")
    (description "Librevenge is a base library for writing document import
filters.  It has interfaces for text documents, vector graphics,
spreadsheets and presentations.")
    (license (list mpl2.0 lgpl2.1+))))            ;dual licensed

(define-public libwpd
  (package
    (name "libwpd")
    (version "0.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpd/" name "/" name "-"
                          version ".tar.xz"))
      (sha256 (base32
               "0b6krzr6kxzm89g6bapn805kdayq70hn16n5b5wfs2lwrf0ag2wx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (home-page "http://libwpd.sourceforge.net/")
    (synopsis "Library for importing WordPerfect documents")
    (description "Libwpd is a C++ library designed to help process
WordPerfect documents.  It is most commonly used to import such documents
into other word processors.")
    (license (list mpl2.0 lgpl2.1+))))            ;dual licensed

(define-public libe-book
  (package
    (name "libe-book")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/libebook/libe-book-"
                            version "/libe-book-" version ".tar.xz"))
        (sha256
          (base32
            "1v48pd32r2pfysr3a3igc4ivcf6vvb26jq4pdkcnq75p70alp2bz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (inputs `(("boost" ,boost)
              ("icu4c" ,icu4c)
              ("librevenge" ,librevenge)
              ("libxml2" ,libxml2)))
    (arguments
     ;; avoid triggering configure errors by simple inclusion of boost headers
     `(#:configure-flags '("--disable-werror")))
    (home-page "http://libebook.sourceforge.net")
    (synopsis "Library for import of reflowable e-book formats")
    (description "Libe-book is a library and a set of tools for reading and
converting various reflowable e-book formats.  Currently supported are:
Broad Band eBook, eReader .pdb, FictionBook v. 2 (including zipped files),
PalmDoc Ebook, Plucker .pdb, QiOO (mobile format, for java-enabled
cellphones), TCR (simple compressed text format), TealDoc, zTXT,
ZVR (simple compressed text format).")
    (license mpl2.0)))

(define-public libwpg
  (package
    (name "libwpg")
    (version "0.3.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpg/" name "/" name "-"
                          version ".tar.xz"))
      (sha256 (base32
               "097jx8a638fwwfrzf6v29r1yhc34rq9526py7wf0ck2z4fcr2w3g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("librevenge" ,librevenge)
       ("libwpd" ,libwpd)
       ("perl" ,perl)
       ("zlib" ,zlib)))
    (home-page "http://libwpg.sourceforge.net/")
    (synopsis "Library and tools for the WordPerfect Graphics format")
    (description "The libwpg project provides a library and tools for
working with graphics in the WPG (WordPerfect Graphics) format.")
    (license (list mpl2.0 lgpl2.1+))))            ;dual licensed

(define-public libabw
  (package
    (name "libabw")
    (version "0.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "0zi1zj4fpxgpglbbb5n1kg3dmhqq5rpf46lli89r5daavp19iing"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("librevenge" ,librevenge)
       ("libxml2" ,libxml2)))
    (arguments
     ;; avoid triggering configure errors by simple inclusion of boost headers
     `(#:configure-flags '("--disable-werror")))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libabw")
    (synopsis "Library for parsing the AbiWord format")
    (description "Libabw is a library that parses the file format of
AbiWord documents.")
    (license mpl2.0)))

(define-public libcdr
  (package
    (name "libcdr")
    (version "0.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "07yzb1yr5kzv0binzj5swz3zzay2gw3xb0fbkc2zwdssgrkf19nh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("icu4c" ,icu4c)
       ("lcms" ,lcms)
       ("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (arguments
     ;; avoid triggering a build failure due to warnings
     `(#:configure-flags '("--disable-werror")))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libcdr")
    (synopsis "Library for parsing the CorelDRAW format")
    (description "Libcdr is a library that parses the file format of
CorelDRAW documents of all versions.")
    (license mpl2.0)))
