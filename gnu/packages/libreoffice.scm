;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Jonathan Brielmaier <jonathan.brielmaier@web.de>
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
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:select (gpl2+ lgpl2.1+ lgpl3+ mpl1.1 mpl2.0
                          non-copyleft x11-style bsd-3))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public ixion
  (package
    (name "ixion")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://kohei.us/files/ixion/src/libixion-"
                           version ".tar.xz"))
       (sha256
        (base32
         "14gdd6div4l22vnz3jn2qjxgjly98ck6p8c1v7386c41rx7kilba"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("mdds" ,mdds)
       ("python" ,python)))
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://kohei.us/files/" name "/src/lib"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1ays13a1x15j81dsrq0d3697v1bbqd3bfz3ajn6kb9d61y2drlgj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("ixion" ,ixion)
       ("mdds" ,mdds)
       ("python" ,python)
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
    (version "0.0.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpd/" name "/" name "-"
                          version "/" name "-" version ".tar.xz"))
      (sha256 (base32
               "1cj76cz4mqcy2mgv9l5xlc95bypyk8zbq0ls9cswqrs2y0lhfgwk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs                  ; Referenced by .la files
     `(("boost" ,boost)))
    (arguments
     ;; avoid triggering configure errors by simple inclusion of boost headers
     `(#:configure-flags '("--disable-werror"
                           ;; Avoid undefined library references
                           "LDFLAGS=-lboost_system")))
    (home-page "https://sourceforge.net/p/libwpd/wiki/librevenge/")
    (synopsis "Document importer for office suites")
    (description "Librevenge is a base library for writing document import
filters.  It has interfaces for text documents, vector graphics,
spreadsheets and presentations.")
    (license (list mpl2.0 lgpl2.1+))))            ;dual licensed

(define-public libwpd
  (package
    (name "libwpd")
    (version "0.10.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpd/libwpd/"
                          "libwpd-" version "/libwpd-" version ".tar.xz"))
      (sha256 (base32
               "0436gnidx45a9vx114hhh216jrh57mqb9zyssyjfadagmyz6hgrj"))))
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
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/libebook/libe-book-"
                            version "/libe-book-" version ".tar.xz"))
        (sha256
          (base32
            "1yg1vws1wggzhjw672bpgh2x541g5i9wryf67g51m0r79zrqz3by"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("icu4c" ,icu4c)
       ("liblangtag" ,liblangtag)
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

(define-public libepubgen
  (package
    (name "libepubgen")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/libepubgen/libepubgen-"
                           version "/libepubgen-" version ".tar.xz"))
       (sha256
        (base32
         "1b8mc9zzrqypj1v9zdy3ybc48pw0rfr06cyi7n6grvybjjwq9q03"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxml2" ,libxml2)
       ("boost" ,boost)))
    (propagated-inputs         ; in Requires field of .pkg
     `(("librevenge" ,librevenge)))
    (home-page "https://sourceforge.net/projects/libepubgen/")
    (synopsis "EPUB generator library for librevenge")
    (description "libepubgen is an EPUB generator for librevenge.  It supports
librevenge's text document interface and--currently in a very limited
way--presentation and vector drawing interfaces.")
    (license mpl2.0)))

(define-public libwpg
  (package
    (name "libwpg")
    (version "0.3.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpg/libwpg/"
                          "libwpg-" version "/libwpg-" version ".tar.xz"))
      (sha256 (base32
               "074x159immf139szkswv2zapnq75p7xk10dbha2p9193hgwggcwr"))))
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
    (version "0.5.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/tdf/libcmis/releases/download/v"
                          version "/libcmis-" version ".tar.xz"))
      (sha256
       (base32
        "18h0a2gsfxvlv03nlcfvw9bzsflq5sin9agq6za103hr0ab8vcfp"))))
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
          ;; During configure, the boost headers are found, but linking
          ;; fails without the following flag.
          (string-append "--with-boost="
                         (assoc-ref %build-inputs "boost")))))
    (home-page "https://github.com/tdf/libcmis")
    (synopsis "CMIS client library")
    (description "LibCMIS is a C++ client library for the CMIS interface.  It
allows C++ applications to connect to any ECM behaving as a CMIS server such
as Alfresco or Nuxeo.")
    (license (list mpl1.1 gpl2+ lgpl2.1+)))) ; triple license

(define-public libabw
  (package
    (name "libabw")
    (version "0.1.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "11949iscdb99f2jplxjd39282jxcrf2fw0sqbh5dl7gqb96r8whb"))))
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
    (version "0.1.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "0j1skr11jwvafn0l6p37v3i4lqc8wcn489g8f7c4mqwbk94mrkka"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("icu4c" ,icu4c)
       ("lcms" ,lcms)
       ("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (inputs
     `(("boost" ,boost)))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libcdr")
    (synopsis "Library for parsing the CorelDRAW format")
    (description "Libcdr is a library that parses the file format of
CorelDRAW documents of all versions.")
    (license mpl2.0)))

(define-public libetonyek
  (package
    (name "libetonyek")
    (version "0.1.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "0jhsbdimiyijdqriy0zzkjjgc4wi6fjimhdg4mdybrlwg7l7f5p6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-mdds=1.4")))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("doxygen" ,doxygen)
       ("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     `(("liblangtag" ,liblangtag)
       ("librevenge" ,librevenge)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
    (inputs
     `(("boost" ,boost)
       ("glm" ,glm)
       ("mdds" ,mdds)))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libetonyek")
    (synopsis "Library for parsing the Apple Keynote format")
    (description "Libetonyek is a library that parses the file format of
Apple Keynote documents.  It currently supports Keynote versions 2 to 5.")
    (license mpl2.0)))

(define-public liblangtag
  (package
    (name "liblangtag")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://bitbucket.org/tagoh/liblangtag/downloads/"
                            name "-" version ".tar.bz2"))
        (sha256
         (base32
          "0bnm4hllr8cfrybm8rw7b8n0nlhzhnv73bkg1bxk452g6a82f96n"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxml2" ,libxml2)))
    ;; As of December 2017, tagoh.bitbucket.org redirects to a hosting advert.
    (home-page "https://bitbucket.org/tagoh/liblangtag")
    (synopsis "Library to access tags for identifying languages")
    (description "Liblangtag implements an interface to work with tags
for identifying languages as described in RFC 5646.  It supports the
extensions described in RFC6067 and RFC6497, and Extension T for
language/locale identifiers as described in the Unicode CLDR
standard 21.0.2.")
    (license (list lgpl3+ mpl2.0)))) ; dual license

(define-public libexttextcat
  (package
    (name "libexttextcat")
    (version "3.4.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "1j6sjwkyhqvsgyw938bxxfwkzzi1mahk66g5342lv6j89jfvrz8k"))))
    (build-system gnu-build-system)
    (home-page "https://www.freedesktop.org/wiki/Software/libexttextcat/")
    (synopsis "Text Categorization library")
    (description "Libexttextcat is an N-Gram-Based Text Categorization
library primarily intended for language guessing.")
    (license (non-copyleft "file://LICENSE"
                           "See LICENSE in the distribution."))))

(define-public libfreehand
  (package
    (name "libfreehand")
    (version "0.1.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "1b1lvqh68rwij1yvmxy02hsmh7i74ma5767mk8mg5nx6chajshhf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("doxygen" ,doxygen)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("icu4c" ,icu4c)
       ("lcms" ,lcms)))
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
    (version "0.1.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "1fhkn013gzg59f4z7rldpbi0nj7lgdqzxanspsqa6axvmahw2dpg"))))
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

(define-public libnumbertext
  (package
    (name "libnumbertext")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Numbertext/libnumbertext/"
                           "releases/download/" version
                           "/libnumbertext-" version ".tar.xz"))
       (sha256
        (base32 "1xzlwhwwhvr76kfdsw5gvfjfdayz803z65h331gv5dpc9imhijg1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (home-page "https://github.com/Numbertext/libnumbertext")
    (synopsis "Language-neutral @code{NUMBERTEXT} and @code{MONEYTEXT} functions")
    (description
     "The libnumbertext library provides language-neutral @code{NUMBERTEXT}
and @code{MONEYTEXT} functions for LibreOffice Calc, available for C++ and
Java.")
    (license (list lgpl3+ bsd-3))))

(define-public libpagemaker
  (package
    (name "libpagemaker")
    (version "0.0.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://dev-www.libreoffice.org/src/libpagemaker/"
                          "libpagemaker-" version ".tar.xz"))
      (sha256 (base32
               "17ai8ajffr0ixxmmcv3k5vgjlcsix38ldb4fw2arild70pbsrbb6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires field of .pkg
     `(("librevenge" ,librevenge)))
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libpagemaker")
    (synopsis "Library for parsing the PageMaker format")
    (description "Libpagemaker is a library that parses the file format of
Aldus/Adobe PageMaker documents.  Currently it only understands documents
created by PageMaker version 6.x and 7.")
    (license mpl2.0)))

(define-public libvisio
  (package
    (name "libvisio")
    (version "0.1.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/libvisio/"
                          "libvisio-" version ".tar.xz"))
      (sha256 (base32
               "1yahpfl13qk6178irv8jn5ppxdn7isafqisyqsdw0lqxcz9h447y"))))
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
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libvisio")
    (synopsis "Library for parsing the Microsoft Visio format")
    (description "Libvisio is a library that parses the file format of
Microsoft Visio documents of all versions.")
    (license mpl2.0)))

(define-public libodfgen
  (package
    (name "libodfgen")
    (version "0.1.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpd/libodfgen/"
                          "libodfgen-" version "/libodfgen-" version ".tar.xz"))
      (sha256 (base32
               "0cdq48wlpp8m0qmndybv64r0m4vh0qsqx69cn6ms533cjlgljgij"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs ; in Requires field of .pkg
     `(("librevenge" ,librevenge)))
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)))
    (arguments
     ;; avoid triggering configure errors by simple inclusion of boost headers
     `(#:configure-flags '("--disable-werror")))
    (home-page "https://sourceforge.net/p/libwpd/wiki/libodfgen/")
    (synopsis "ODF (Open Document Format) library")
    (description "Libodfgen is a library for generating documents in the
Open Document Format (ODF).  It provides generator implementations for all
document interfaces supported by librevenge:
text documents, vector drawings, presentations and spreadsheets.")
    (license (list mpl2.0 lgpl2.1+)))) ; dual license

(define-public libmwaw
  (package
    (name "libmwaw")
    (version "0.3.15")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libmwaw/libmwaw/libmwaw-"
                          version "/libmwaw-" version ".tar.xz"))
      (sha256 (base32
               "1cdhm9yhanyv3w4vr73zhgyynmkhhkp3dyld7m11jd2yy04vnh04"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs                  ; in Requires field of .pkg
     `(("librevenge" ,librevenge)))
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)))
    (home-page "https://sourceforge.net/p/libmwaw/wiki/Home/")
    (synopsis "Import library for some old Macintosh text documents")
    (description "Libmwaw contains some import filters for old Macintosh
text documents (MacWrite, ClarisWorks, ... ) and for some graphics and
spreadsheet documents.")
    (license (list mpl2.0 lgpl2.1+))))  ; dual license

(define-public libstaroffice
  (package
    (name "libstaroffice")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fosnola/libstaroffice/releases/download/"
                           version "/libstaroffice-" version ".tar.xz"))
       (sha256 (base32
                "1i0ykl0c94lc1qzb5mbyf9jr7qw8p38ja424whmhgrllh7ny203b"))))
    (build-system gnu-build-system)
    (inputs
     `(("librevenge" ,librevenge)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/fosnola/libstaroffice")
    (synopsis "Provides LibreOffice support for old StarOffice documents")
    (description "@code{libstaroffice} is an import filter for the document formats
from the old StarOffice (.sdc, .sdw, ...).")
    (license (list mpl2.0 lgpl2.1+)))) ; dual license

(define-public libwps
  (package
    (name "libwps")
    (version "0.4.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/" name "/" name "/"
                          name "-" version "/" name "-" version ".tar.xz"))
      (sha256 (base32
               "1ji9zd4wxmas03g8jyx0ih0amrqfazm5874a2v9rd7va50sf088l"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs                  ; in Requires field of .pkg
     `(("librevenge" ,librevenge)))
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)))
    (home-page "http://libwps.sourceforge.net/")
    (synopsis "Import library for Microsoft Works text documents")
    (description "Libwps is a library for importing files in the Microsoft
Works word processor file format.")
    (license (list mpl2.0 lgpl2.1+))))  ; dual license

(define-public libzmf
  (package
   (name "libzmf")
   (version "0.0.2")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/libzmf/libzmf-"
                          version ".tar.xz"))
      (sha256 (base32
               "08mg5kmkjrmqrd8j5rkzw9vdqlvibhb1ynp6bmfxnzq5rcq1l197"))))
   (build-system gnu-build-system)
   (arguments
    ;; A harmless 'sign-compare' error pops up on i686 so disable '-Werror'.
    '(#:configure-flags '("--disable-werror")))
   (inputs
    `(("boost" ,boost)
      ("icu4c" ,icu4c)
      ("libpng" ,libpng)
      ("librevenge" ,librevenge)
      ("zlib" ,zlib)))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libzmf")
    (synopsis "Parses file format of Zoner Callisto/Draw documents")
    (description "Libzmf is a library that parses the file format of Zoner
Callisto/Draw documents.  Currently it only understands documents created by
Zoner Draw version 4 and 5.")
    (license mpl2.0)))

(define-public hunspell
  (package
    (name "hunspell")
    (version "1.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hunspell/hunspell")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0qxlkd012r45ppd21kldbq9k5ac5nmxz290z6m2kch9l56v768k1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("perl" ,perl)))
    (native-search-paths (list (search-path-specification
                                (variable "DICPATH")
                                (files '("share/hunspell")))))
    (home-page "https://hunspell.github.io/")
    (synopsis "Spell checker")
    (description "Hunspell is a spell checker and morphological analyzer
library and program designed for languages with rich morphology and complex
word compounding or character encoding.")
    ;; Triple license, including "mpl1.1 or later".
    (license (list mpl1.1 gpl2+ lgpl2.1+))))

(define (dicollecte-french-dictionary variant synopsis)
  ;; Return a French dictionary package from dicollecte.org, for the given
  ;; VARIANT.
  (package
    (name (match variant
            ("classique" "hunspell-dict-fr")
            (_ (string-append "hunspell-dict-fr-" variant))))
    (version "6.2")
    (source (origin
              (uri (string-append
                    "http://www.dicollecte.org/download/fr/hunspell-french-dictionaries-v"
                    version ".zip"))
              (method url-fetch)
              (sha256
               (base32
                "139hfrn5p87sl8hqmgkf6sgvnxrk2mm8vd8xsm8sm98qjnwlg0f9"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))

                   (let* ((out      (assoc-ref %outputs "out"))
                          (hunspell (string-append out "/share/hunspell"))
                          (myspell  (string-append out "/share/myspell"))
                          (doc      (string-append out "/share/doc/"
                                                   ,name))
                          (unzip    (assoc-ref %build-inputs "unzip")))
                     (invoke (string-append unzip "/bin/unzip")
                             (assoc-ref %build-inputs "source"))
                     (for-each (cut install-file <> hunspell)
                               (find-files "."
                                           ,(string-append variant
                                                           "\\.(dic|aff)$")))
                     (mkdir-p myspell)
                     (symlink hunspell (string-append myspell "/dicts"))
                     (for-each (cut install-file <> doc)
                               (find-files "." "\\.(txt|org|md)$"))
                     #t))))
    (synopsis synopsis)
    (description
     "This package provides a dictionary for the Hunspell spell-checking
library.")
    (home-page "https://www.dicollecte.org/home.php?prj=fr")
    (license mpl2.0)))

(define-syntax define-french-dictionary
  (syntax-rules (synopsis)
    ((_ name variant (synopsis text))
     (define-public name
       (dicollecte-french-dictionary variant text)))))

(define-french-dictionary hunspell-dict-fr-classique
  "classique"
  ;; TRANSLATORS: In French, this is "Français classique".
  (synopsis "Hunspell dictionary for ``classic'' French (recommended)"))

(define-french-dictionary hunspell-dict-fr-moderne
  "moderne"
  ;; TRANSLATORS: In French, this is "Français moderne".
  (synopsis "Hunspell dictionary for ``modern'' French"))

(define-french-dictionary hunspell-dict-fr-réforme-1990
  "reforme1990"
  (synopsis "Hunspell dictionary for the post @dfn{1990 réforme} French"))

(define-french-dictionary hunspell-dict-fr-toutes-variantes
  "toutesvariantes"
  (synopsis "Hunspell dictionary for all variants of French"))

(define-public hyphen
  (package
    (name "hyphen")
    (version "2.8.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/hunspell/Hyphen/"
                          (version-major+minor version) "/"
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
      (uri (string-append "mirror://sourceforge/hunspell/MyThes/" version "/"
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
and to return information on pronunciations, meanings and synonyms.")
    (license (non-copyleft "file://COPYING"
                           "See COPYING in the distribution."))))

(define-public libqxp
  (package
    (name "libqxp")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dev-www.libreoffice.org/src/libqxp/"
                                  "libqxp-" version ".tar.xz"))
              (sha256
               (base32
                "0p4lb84m05wqd8qr8ni9sp80ivlm83ffn0nxiv4m42hj22qvcdz1"))))
    (build-system gnu-build-system)
    (inputs
     `(("boost" ,boost)
       ("icu4c" ,icu4c)
       ("zlib" ,zlib)))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("librevenge" ,librevenge))) ; mentioned in Requires field
    (home-page "https://www.libreoffice.org")
    (synopsis "Library and tools for the QuarkXPress file format")
    (description "libqxp is a library and a set of tools for reading and
converting QuarkXPress file format.  It supports versions 3.1 to 4.1.")
    (license mpl2.0)))

(define-public libreoffice
  (package
    (name "libreoffice")
    (version "6.1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://download.documentfoundation.org/libreoffice/src/"
         (version-prefix version 3) "/libreoffice-" version ".tar.xz"))
       (sha256
        (base32
         "1wh8qhqkmb89nmfcb0w6iwpdzxwqr7c5kzxgpk4gy60xin6gwjgb"))
       (patches
        (append (list (origin
                        ;; Support newer versions of Orcus and MDDS.  These patches
                        ;; are taken from upstream, but we use the patches from Arch
                        ;; because they are adapted for the release tarball.
                        ;; Note: remove the related substitutions below when these
                        ;; are no longer needed.
                        (method url-fetch)
                        (uri (string-append "https://git.archlinux.org/svntogit"
                                            "/packages.git/plain/trunk/"
                                            "0001-Update-orcus-to-0.14.0.patch?&id="
                                            "4002fa927f2a143bd2ec008a0c400b2ce9f2c8a7"))
                        (file-name "libreoffice-orcus.patch")
                        (sha256
                         (base32
                          "0v1knblrmfzkb4g9pm5mdnrmjib59bznvca1ygbwlap2ln1h4mk0")))
                      (origin
                        (method url-fetch)
                        (uri (string-append "https://git.archlinux.org/svntogit"
                                            "/packages.git/plain/trunk/"
                                            "0001-Update-mdds-to-1.4.1.patch?&id="
                                            "4002fa927f2a143bd2ec008a0c400b2ce9f2c8a7"))
                        (file-name "libreoffice-mdds.patch")
                        (sha256
                         (base32
                          "0apbmammmp4pk473xiv5vk50r4c5gjvqzf9jkficksvz58q6114f"))))
                (search-patches "libreoffice-boost.patch"
                                "libreoffice-icu.patch"
                                "libreoffice-glm.patch")))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each (lambda (file)
                       ;; Adjust to renamed function in Poppler 0.72.
                       (substitute* file (("getCString") "c_str")))
                     (find-files "sdext/source/pdfimport/xpdfwrapper"))
           #t))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("cppunit" ,cppunit-1.14)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("which" ,which)))
    (inputs
     `(("bluez" ,bluez)
       ("boost" ,boost)
       ("clucene" ,clucene)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("fontconfig" ,fontconfig)
       ("gconf" ,gconf)
       ("glew" ,glew)
       ("glm" ,glm)
       ("gperf" ,gperf)
       ("gpgme" ,gpgme)
       ("graphite2" ,graphite2)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("harfbuzz" ,harfbuzz)
       ("hunspell" ,hunspell)
       ("hyphen" ,hyphen)
       ("libabw" ,libabw)
       ("libcdr" ,libcdr)
       ("libcmis" ,libcmis)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libe-book" ,libe-book)
       ("libepubgen" ,libepubgen)
       ("libetonyek" ,libetonyek)
       ("libexttextcat" ,libexttextcat)
       ("libfreehand" ,libfreehand)
       ("liblangtag" ,liblangtag)
       ;; XXX: Perhaps this should be propagated from xmlsec.
       ("libltdl" ,libltdl)
       ("libmspub" ,libmspub)
       ("libmwaw" ,libmwaw)
       ("libnumbertext" ,libnumbertext)
       ("libodfgen" ,libodfgen)
       ("libpagemaker" ,libpagemaker)
       ("libqxp" ,libqxp)
       ("libstaroffice" ,libstaroffice)
       ("libvisio" ,libvisio)
       ("libwpg" ,libwpg)
       ("libwps" ,libwps)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxt" ,libxt)
       ("libzmf" ,libzmf)
       ("lpsolve" ,lpsolve)
       ("mdds" ,mdds)
       ("mythes" ,mythes)
       ("neon" ,neon)
       ("nspr" ,nspr)
       ("nss" ,nss)
       ("openldap" ,openldap)
       ("openssl" ,openssl)
       ("orcus" ,orcus)
       ("perl" ,perl)
       ("perl-archive-zip" ,perl-archive-zip)
       ("poppler" ,poppler)
       ("postgresql" ,postgresql)
       ("python" ,python)
       ("python-lxml" ,python-lxml)
       ("redland" ,redland)
       ("sane-backends" ,sane-backends)
       ("unixodbc" ,unixodbc)
       ("unzip" ,unzip)
       ("vigra" ,vigra)
       ("xmlsec" ,xmlsec-nss)
       ("zip" ,zip)))
    (arguments
     `(#:tests? #f ; Building the tests already fails.
       #:make-flags '("build-nocheck") ; Do not build unit tests, which fails.
       #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'prepare-src
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute*
                   (list "sysui/CustomTarget_share.mk"
                         "solenv/gbuild/gbuild.mk"
                         "solenv/gbuild/platform/unxgcc.mk")
                 (("/bin/sh") (which "sh")))

               ;; XXX: Adjust the checks for MDDS and liborcus to avoid having
               ;; to re-bootstrap the whole thing.  Remove this with the related
               ;; patches above.
               (substitute* "configure"
                 (("mdds-1.2 >= 1.2.3") "mdds-1.4 >= 1.4.1")
                 (("liborcus-0.13 >= 0.13.3") "liborcus-0.14 >= 0.14.0"))

               ;; GPGME++ headers are installed in a gpgme++ subdirectory, but
               ;; files in "xmlsecurity/source/gpg/" and elsewhere expect to
               ;; find them on the include path without a prefix.
               (substitute* '("xmlsecurity/Library_xsec_xmlsec.mk"
                              "comphelper/Library_comphelper.mk")
                 (("\\$\\$\\(INCLUDE\\)")
                  (string-append "$$(INCLUDE) -I"
                                 (assoc-ref inputs "gpgme")
                                 "/include/gpgme++")))

               #t))
           (add-after 'install 'bin-and-desktop-install
             ;; Create 'soffice' and 'libreoffice' symlinks to the executable
             ;; script.
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (define (symlink-output src dst)
                   (mkdir-p (dirname (string-append out dst)))
                   (symlink (string-append out src) (string-append out dst)))
                 (define (install src dst)
                   (let ((dst (string-append out dst)))
                     (mkdir-p (dirname dst))
                     (copy-file src dst)))
                 (define (install-desktop-file app)
                   (let ((src (string-append "/lib/libreoffice/share/xdg/"
                                             app ".desktop"))
                         (dst (string-append "/share/applications/libreoffice-"
                                             app ".desktop")))
                     (substitute* (string-append out src)
                       (("Exec=libreoffice[0-9]+\\.[0-9]+ ")
                        (string-append "Exec=" out "/bin/libreoffice "))
                       (("Icon=libreoffice.*")
                        (string-append "Icon=" app "\n"))
                       (("LibreOffice [0-9]+\\.[0-9]+")
                        "LibreOffice"))
                     (symlink-output src dst)))
                 (define (install-appdata app)
                   (install-file (string-append
                                    "sysui/desktop/appstream-appdata/"
                                    "libreoffice-" app ".appdata.xml")
                                   (string-append out "/share/appdata")))
                 (symlink-output "/lib/libreoffice/program/soffice"
                                 "/bin/soffice")
                 (symlink-output "/lib/libreoffice/program/soffice"
                                 "/bin/libreoffice")
                 (install "workdir/CustomTarget/sysui/share/libreoffice/openoffice.keys"
                          "/share/mime-info/libreoffice.keys")
                 (install "workdir/CustomTarget/sysui/share/libreoffice/openoffice.mime"
                          "/share/mime-info/libreoffice.mime")
                 (install
                  "workdir/CustomTarget/sysui/share/libreoffice/openoffice.org.xml"
                  "/share/mime/packages/libreoffice.xml")
                 (for-each install-desktop-file
                           '("base" "calc" "draw" "impress" "writer"
                             "math" "startcenter"))
                 (for-each install-appdata
                           '("base" "calc" "draw" "impress" "writer"))
                 (mkdir-p (string-append out "/share/icons/hicolor"))
                 (copy-recursively "sysui/desktop/icons/hicolor"
                                   (string-append out "/share/icons/hicolor")))
               #t)))
       #:configure-flags
        (list
          "--enable-release-build"
          "--enable-verbose"
          ;; Avoid using all cpu cores by default
          (format #f "--with-parallelism=~d" (parallel-job-count))
          "--disable-fetch-external" ; disable downloads
          "--with-system-libs" ; enable all --with-system-* flags
          (string-append "--with-boost-libdir="
                         (assoc-ref %build-inputs "boost") "/lib")
          ;; Avoid undefined symbols required by boost::spirit
          "LDFLAGS=-lboost_system"
          ;; Avoid a dependency on ucpp.
          "--with-idlc-cpp=cpp"
          ;; The fonts require an external tarball (crosextrafonts).
          ;; They should not be needed when system fonts are available.
          "--without-fonts"
          ;; With java, the build fails since sac.jar is missing.
          "--without-java"
          ;; FIXME: Enable once the corresponding inputs are packaged.
          "--without-system-npapi-headers"
          "--disable-coinmp"
          "--disable-firebird-sdbc" ; embedded firebird
          "--disable-gltf"
          ;; XXX: PDFium support requires fetching an external tarball and
          ;; patching the build scripts to work with GCC5.  Try enabling this
          ;; when our default compiler is >=GCC 6.
          "--disable-pdfium"
          "--disable-gtk" ; disable use of GTK+ 2
          "--without-doxygen")))
    (home-page "https://www.libreoffice.org/")
    (synopsis "Office suite")
    (description "LibreOffice is a comprehensive office suite.  It contains
a number of components: Writer, a word processor; Calc, a spreadsheet
application; Impress, a presentation engine; Draw, a drawing and
flowcharting application; Base, a database and database frontend;
Math for editing mathematics.")
    (license mpl2.0)))
