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
  #:use-module ((guix licenses)
                #:select (gpl2+ lgpl2.1+ mpl1.1 mpl2.0 non-copyleft))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages doxygen)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages openssl)
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
    (propagated-inputs
     `(("librevenge" ,librevenge))) ; in Requires field of .pkg
    (inputs
     `(("zlib" ,zlib)))
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
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("icu4c" ,icu4c)
       ("librevenge" ,librevenge)
       ("libxml2" ,libxml2)))
    (inputs
      `(("boost" ,boost)))
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
    (propagated-inputs
     `(("libwpd" ,libwpd))) ; in Requires field of .pkg
    (inputs
     `(("perl" ,perl)
       ("zlib" ,zlib)))
    (home-page "http://libwpg.sourceforge.net/")
    (synopsis "Library and tools for the WordPerfect Graphics format")
    (description "The libwpg project provides a library and tools for
working with graphics in the WPG (WordPerfect Graphics) format.")
    (license (list mpl2.0 lgpl2.1+))))            ;dual licensed

(define-public libcmis
  (package
    (name "libcmis")
    (version "0.5.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/" name "/" name "-"
                          version ".tar.gz"))
      (sha256 (base32
               "1dprvk4fibylv24l7gr49gfqbkfgmxynvgssvdcycgpf7n8h4zm8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires field of .pkg
     `(("curl" ,curl)
       ("libxml2" ,libxml2)))
    (inputs
     `(("boost" ,boost)
       ("cyrus-sasl" ,cyrus-sasl)
       ("openssl" ,openssl)))
    (arguments
     `(#:configure-flags
        (list
          ;; FIXME: Man pages generation requires docbook-to-man; reenable
          ;; it once this is available.
          "--without-man"
          ;; avoid triggering configure errors by simple inclusion of
          ;; boost headers
          "--disable-werror"
          ;; During configure, the boost headers are found, but linking
          ;; fails without the following flag.
          (string-append "--with-boost="
                         (assoc-ref %build-inputs "boost")))))
    (home-page "http://sourceforge.net/projects/libcmis/")
    (synopsis "CMIS client library")
    (description "LibCMIS is a C++ client library for the CMIS interface.  It
allows C++ applications to connect to any ECM behaving as a CMIS server such
as Alfresco or Nuxeo.")
    (license (list mpl1.1 gpl2+ lgpl2.1+)))) ; triple license

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
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("librevenge" ,librevenge)
       ("libxml2" ,libxml2)))
    (inputs
     `(("boost" ,boost)))
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
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("icu4c" ,icu4c)
       ("lcms" ,lcms)
       ("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (inputs
     `(("boost" ,boost)))
    (arguments
     ;; avoid triggering a build failure due to warnings
     `(#:configure-flags '("--disable-werror")))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libcdr")
    (synopsis "Library for parsing the CorelDRAW format")
    (description "Libcdr is a library that parses the file format of
CorelDRAW documents of all versions.")
    (license mpl2.0)))

(define-public libetonyek
  (package
    (name "libetonyek")
    (version "0.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "0gn8v24jb9r9kxppbws6xlc7knpd9mk2n9xjvziccv5f2l7mlslw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("doxygen" ,doxygen)
       ("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("librevenge" ,librevenge)
       ("libxml2" ,libxml2)))
    (inputs
     `(("boost" ,boost)))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libetonyek")
    (synopsis "Library for parsing the Apple Keynote format")
    (description "Libetonyek is a library that parses the file format of
Apple Keynote documents.  It currently supports Keynote versions 2 to 5.")
    (license mpl2.0)))

(define-public libexttextcat
  (package
    (name "libexttextcat")
    (version "3.4.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "14v2hkygnmf1zgahfm1fha47cr67iikrz2ymiqi28d2jydn0hk7j"))))
    (build-system gnu-build-system)
    (home-page "http://www.freedesktop.org/wiki/Software/libexttextcat/")
    (synopsis "Text Categorization library")
    (description "Libexttextcat is an N-Gram-Based Text Categorization
library primarily intended for language guessing.")
    (license (non-copyleft "file://LICENSE"
                           "See LICENSE in the distribution."))))

(define-public libfreehand
  (package
    (name "libfreehand")
    (version "0.1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "01j7mxi4lmf72w1mv2r098p8l0csdd94w2gq0ncp93djn34al6ai"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libfreehand")
    (synopsis "Library for parsing the FreeHand format")
    (description "Libfreehand is a library that parses the file format of
Aldus/Macromedia/Adobe FreeHand documents.")
    (license mpl2.0)))

(define-public libmspub
  (package
    (name "libmspub")
    (version "0.1.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "03sn6lxpr49sdq6j8q7fw7yjybyfahhs03z80388mh105pwapfmh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("icu4c" ,icu4c)
       ("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (inputs
     `(("boost" ,boost)))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libmspub")
    (synopsis "Library for parsing the Microsoft Publisher format")
    (description "Libmspub is a library that parses the file format of
Microsoft Publisher documents of all versions.")
    (license mpl2.0)))

(define-public libpagemaker
  (package
    (name "libpagemaker")
    (version "0.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "05zgj5ngg9z4b7dnrfs59nm0macm99lzyxv4mg53jcvp0mkgigfd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (native-inputs ; in Requires field of .pkg
     `(("librevenge" ,librevenge)))
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)))
    (arguments
     ;; avoid triggering a build failure due to warnings
     `(#:configure-flags '("--disable-werror")))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libpagemaker")
    (synopsis "Library for parsing the PageMaker format")
    (description "Libpagemaker is a library that parses the file format of
Aldus/Adobe PageMaker documents.  Currently it only understands documents
created by PageMaker version 6.x and 7.")
    (license mpl2.0)))

(define-public libvisio
  (package
    (name "libvisio")
    ;; FIXME: The newer version 0.1.1 fails its tests.
    (version "0.1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "0vvd2wyp4rw6s9xnj1dc9vgdpfvm45gnb5b9hhzif0fdnx4iskqf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("doxygen" ,doxygen)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("icu4c" ,icu4c)
       ("librevenge" ,librevenge)
       ("libxml2" ,libxml2)))
    (inputs
     `(("boost" ,boost)))
    ;; FIXME: Not needed any more for newer version 0.1.1.
    (arguments
     ;; avoid triggering a build failure due to warnings
     `(#:configure-flags '("--disable-werror")))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libvisio")
    (synopsis "Library for parsing the Microsoft Visio format")
    (description "Libvisio is a library that parses the file format of
Microsoft Visio documents of all versions.")
    (license mpl2.0)))

(define-public libodfgen
  (package
    (name "libodfgen")
    (version "0.1.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/"
                          name "-" version ".tar.bz2"))
      (sha256 (base32
               "074qsav86ixwi9zm1f77g9vxdf1ihm6n930vxjg8q3lwzd8g7lb6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (arguments
     ;; avoid triggering configure errors by simple inclusion of boost headers
     `(#:configure-flags '("--disable-werror")))
    (home-page "http://sourceforge.net/p/libwpd/wiki/libodfgen/")
    (synopsis "ODF (Open Document Format) library")
    (description "Libodfgen is a library for generating documents in the
Open Document Format (ODF).  It provides generator implementations for all
document interfaces supported by librevenge:
text documents, vector drawings, presentations and spreadsheets.")
    (license (list mpl2.0 lgpl2.1+)))) ; dual license

(define-public libmwaw
  (package
    (name "libmwaw")
    (version "0.3.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/" name "/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "1sn95flxrh85qjsg1kk700c1ggxaaccr9j1nnw7x4daw8lky25ac"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (arguments
     ;; avoid triggering configure errors by simple inclusion of boost headers
     `(#:configure-flags '("--disable-werror")))
    (home-page "http://sourceforge.net/p/libmwaw/wiki/Home/")
    (synopsis "Import library for some old Macintosh text documents")
    (description "Libmwaw contains some import filters for old Macintosh
text documents (MacWrite, ClarisWorks, ... ) and for some graphics and
spreadsheet documents.")
    (license (list mpl2.0 lgpl2.1+)))) ; dual license

(define-public libwps
  (package
    (name "libwps")
    (version "0.3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/" name "/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "14wfhw1ahavhx4hrdzc4hdwxjlffrm939kswf2x1250jnmyjlb5v"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (arguments
     ;; avoid triggering configure errors by simple inclusion of boost headers
     `(#:configure-flags '("--disable-werror")))
    (home-page "http://libwps.sourceforge.net/")
    (synopsis "Import library for Microsoft Works text documents")
    (description "Libwps is a library for importing files in the Microsoft
Works word processor file format.")
    (license (list mpl2.0 lgpl2.1+)))) ; dual license

(define-public hunspell
  (package
    (name "hunspell")
    (version "1.3.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/" name "/"
                          name "-" version ".tar.gz"))
      (sha256 (base32
               "0v14ff9s37vkh45diaddndcrj0hmn67arh8xh8k79q9c1vgc1cm7"))))
    (build-system gnu-build-system)
    (home-page "http://hunspell.sourceforge.net/")
    (synopsis "Spell checker")
    (description "Hunspell is a spell checker and morphological analyzer
library and program designed for languages with rich morphology and complex
word compounding or character encoding.")
    ;; triple license, including "mpl1.1 or later"
    (license (list mpl1.1 gpl2+ lgpl2.1+))))

(define-public hyphen
  (package
    (name "hyphen")
    (version "2.8.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/hunspell/"
                          name "-" version ".tar.gz"))
      (sha256 (base32
               "01ap9pr6zzzbp4ky0vy7i1983fwyqy27pl0ld55s30fdxka3ciih"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)))
    (home-page "http://hunspell.sourceforge.net/")
    (synopsis "Hyphenation library")
    (description "Hyphen is a hyphenation library using TeX hyphenation
patterns, which are pre-processed by a perl script.")
    ;; triple license, including "mpl1.1 or later"
    (license (list mpl1.1 mpl2.0 gpl2+ lgpl2.1+))))

(define-public mythes
  (package
    (name "mythes")
    (version "1.2.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/hunspell/"
                          name "-" version ".tar.gz"))
      (sha256 (base32
               "0prh19wy1c74kmzkkavm9qslk99gz8h8wmjvwzjc6lf8v2az708y"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("hunspell" ,hunspell)
       ("perl" ,perl)))
    (home-page "http://hunspell.sourceforge.net/")
    (synopsis "Thesaurus")
    (description "MyThes is a simple thesaurus that uses a structured text
data file and an index file with binary search to look up words and phrases
and to return information on pronunciations, meaningss and synonyms.")
    (license (non-copyleft "file://COPYING"
                           "See COPYING in the distribution."))))
