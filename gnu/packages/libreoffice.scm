;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2019 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2020 Marcin Karpezo <sirmacik@wioo.waw.pl>
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
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages aspell)
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
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
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
  #:use-module (gnu packages logging)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public ixion
  (package
    (name "ixion")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://kohei.us/files/ixion/src/libixion-"
                           version ".tar.xz"))
       (sha256
        (base32
         "17q84mhy4rb3masvjw24x549irdjmccnc8n04xh58v9l7hxn8v22"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list mdds python spdlog))
    (home-page "https://gitlab.com/ixion/ixion")
    (synopsis "General purpose formula parser and interpreter")
    (description "Ixion is a library for calculating the results of formula
expressions stored in multiple named targets, or \"cells\".  The cells can
be referenced from each other, and the library takes care of resolving
their dependencies automatically upon calculation.")
    (license license:mpl2.0)))

(define-public orcus
  (package
    (name "orcus")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://kohei.us/files/orcus/src/lib"
                           "orcus-" version ".tar.xz"))
       (sha256
        (base32
         "1bps34sqz7wlrl01ssywjd5fbmssplifs0rskivgrg801lr6pcm4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list ixion mdds python zlib))
    (home-page "https://gitlab.com/orcus/orcus")
    (synopsis "File import filter library for spreadsheet documents")
    (description "Orcus is a library that provides a collection of standalone
file processing filters.  It is currently focused on providing filters for
spreadsheet documents.  The library includes import filters for
Microsoft Excel 2007 XML, Microsoft Excel 2003 XML, Open Document Spreadsheet,
Plain Text, Gnumeric XML, Generic XML.  It also includes low-level parsers for
CSV, CSS and XML.")
    (license license:mpl2.0)))

(define-public unoconv
  (package
    (name "unoconv")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "unoconv" version))
       (sha256
        (base32 "0cb0bvyxib3xrj0jdgizhp6p057lr8kqnd3n921rin37ivcvz3ih"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'stop-hash-sniffing
           ;; Fixes <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39647#11>.
           ;; Submitted upsteam: <https://github.com/unoconv/unoconv/pull/531>.
           (lambda _
             (substitute* "unoconv"
               (("sys.argv\\[0\\]\\.split\\('2'\\)")
                "os.path.basename(sys.argv[0]).split('2')"))
             #t))
         (add-after 'unpack 'patch-find_offices
           ;; find_offices is a convoluted cross-platform treasure hunt.
           ;; Keep things simple and return the correct paths immediately.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((libreoffice (assoc-ref inputs "libreoffice")))
               (substitute* "unoconv"
                 (("def find_offices\\(\\):" match)
                  (string-append
                   match "\n"
                   "    return [Office("
                   "'" libreoffice "/lib/libreoffice', "
                   "'" libreoffice "/lib/libreoffice/program', "
                   "'" libreoffice "/lib/libreoffice/program', "
                   "'" libreoffice "/lib/libreoffice/program/pyuno.so', "
                   "'" libreoffice "/bin/soffice', "
                   "sys.executable, "
                   "None)]\n")))
               #t))))))
    (inputs
     (list libreoffice))
    (home-page "http://dag.wiee.rs/home-made/unoconv/")
    (synopsis "Convert between any document format supported by LibreOffice")
    (description
     "Unoconv is a command-line utility to convert documents from any format
that LibreOffice can import, to any format it can export.  It can be used for
batch processing and can apply custom style templates and filters.

Unoconv converts between over a hundred formats, including Open Document
Format (@file{.odt}, @file{.ods}, @file{.odp})), Portable Document Format
(@file{.pdf}), HTML and XHTML, RTF, DocBook (@file{.xml}), @file{.doc} and
@file{.docx}), @file{.xls} and @file{.xlsx}).

All required fonts must be installed on the converting system.")
    (license license:gpl2)))

(define-public librevenge
  (package
    (name "librevenge")
    (version "0.0.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpd/librevenge/librevenge-"
                          version "/librevenge-" version ".tar.xz"))
      (sha256 (base32
               "1cj76cz4mqcy2mgv9l5xlc95bypyk8zbq0ls9cswqrs2y0lhfgwk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list cppunit doxygen pkg-config))
    (inputs
     (list zlib))
    (propagated-inputs                  ; Referenced by .la files
     (list boost))
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
    (license (list license:mpl2.0 license:lgpl2.1+)))) ; dual-licensed

(define-public libwpd
  (package
    (name "libwpd")
    (version "0.10.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpd/libwpd/"
                          "libwpd-" version "/libwpd-" version ".tar.xz"))
      (sha256 (base32
               "02fx8bngslcj7i5g1gx2kiign4vp09wrmp5wpvix9igxcavb0r94"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-werror")))
    (native-inputs
     (list doxygen pkg-config))
    (propagated-inputs
     (list librevenge)) ; in Requires field of .pkg
    (inputs
     (list zlib))
    (home-page "http://libwpd.sourceforge.net/")
    (synopsis "Library for importing WordPerfect documents")
    (description "Libwpd is a C++ library designed to help process
WordPerfect documents.  It is most commonly used to import such documents
into other word processors.")
    (license (list license:mpl2.0 license:lgpl2.1+)))) ; dual-licensed

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
          "1yg1vws1wggzhjw672bpgh2x541g5i9wryf67g51m0r79zrqz3by"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; This can be removed with the next release.
            ;; Needed for icu4c compatibility >= 68.0.
            (substitute* "src/lib/EBOOKCharsetConverter.cpp"
              (("TRUE, TRUE, &status")
              "true, true, &status"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list cppunit gperf pkg-config))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     (list icu4c liblangtag librevenge libxml2))
    (inputs
      (list boost))
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
    (license license:mpl2.0)))

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
     (list cppunit pkg-config))
    (inputs
     (list libxml2 boost))
    (propagated-inputs         ; in Requires field of .pkg
     (list librevenge))
    (home-page "https://sourceforge.net/projects/libepubgen/")
    (synopsis "EPUB generator library for librevenge")
    (description "libepubgen is an EPUB generator for librevenge.  It supports
librevenge's text document interface and--currently in a very limited
way--presentation and vector drawing interfaces.")
    (license license:mpl2.0)))

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
     (list doxygen pkg-config))
    (propagated-inputs
     (list libwpd)) ; in Requires field of .pkg
    (inputs
     (list perl zlib))
    (home-page "http://libwpg.sourceforge.net/")
    (synopsis "Library and tools for the WordPerfect Graphics format")
    (description "The libwpg project provides a library and tools for
working with graphics in the WPG (WordPerfect Graphics) format.")
    (license (list license:mpl2.0 license:lgpl2.1+)))) ; dual-licensed

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
     (list cppunit pkg-config))
    (propagated-inputs ; in Requires field of .pkg
     (list curl libxml2))
    (inputs
     (list boost cyrus-sasl openssl))
    (arguments
     `(#:configure-flags
        (list
          ;; FIXME: Man pages generation requires docbook-to-man; reenable
          ;; it once this is available.
          "--without-man"
          ;; XXX: A configure test fails with GCC7 when including Boost headers.
          "--disable-werror"
          ;; During configure, the boost headers are found, but linking
          ;; fails without the following flag.
          (string-append "--with-boost="
                         (assoc-ref %build-inputs "boost")))))
    (home-page "https://github.com/tdf/libcmis")
    (synopsis "CMIS client library")
    (description "LibCMIS is a C++ client library for the CMIS interface.  It
allows C++ applications to connect to any ECM behaving as a CMIS server such
as Alfresco or Nuxeo.")
    (license
     (list license:mpl1.1 license:gpl2+ license:lgpl2.1+)))) ; triple license

(define-public libabw
  (package
    (name "libabw")
    (version "0.1.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://dev-www.libreoffice.org/src/libabw/"
                          "libabw-" version ".tar.xz"))
      (sha256 (base32
               "1vbfrmnvib3cym0yyyabnd8xpx4f7wp20vnn09s6dln347fajqz7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list doxygen gperf perl pkg-config))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     (list librevenge libxml2))
    (inputs
     (list boost))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libabw")
    (synopsis "Library for parsing the AbiWord format")
    (description "Libabw is a library that parses the file format of
AbiWord documents.")
    (license license:mpl2.0)))

(define-public libcdr
  (package
    (name "libcdr")
    (version "0.1.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "1m6dirmyhqwnrpv80z97x5k5hdh4kh8a8zlq3smbjrilc6fj8rjn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list cppunit doxygen pkg-config))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     (list icu4c lcms librevenge zlib))
    (inputs
     (list boost))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libcdr")
    (synopsis "Library for parsing the CorelDRAW format")
    (description "Libcdr is a library that parses the file format of
CorelDRAW documents of all versions.")
    (license license:mpl2.0)))

(define-public libetonyek
  (package
    (name "libetonyek")
    (version "0.1.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256 (base32
               "16hy60ws29pb4pz3z5l4920yn9hnk2vlij0xfs5qi1w4drd46c5l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-mdds=1.5")))
    (native-inputs
     (list cppunit doxygen gperf pkg-config))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     (list liblangtag librevenge libxml2 zlib))
    (inputs
     (list boost glm mdds))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libetonyek")
    (synopsis "Library for parsing the Apple Keynote format")
    (description "Libetonyek is a library that parses the file format of
Apple Keynote documents.  It currently supports Keynote versions 2 to 5.")
    (license license:mpl2.0)))

(define-public liblangtag
  (package
    (name "liblangtag")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://bitbucket.org/tagoh/liblangtag/downloads/"
                            "liblangtag-" version ".tar.bz2"))
        (sha256
         (base32 "1g9kwxx60q0hpwvs66ys1cb9qg54hfvbivadwli8sfpc085a44hz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list libtool pkg-config))
    (inputs
     (list libxml2))
    (home-page "https://bitbucket.org/tagoh/liblangtag")
    (synopsis "Library to access tags for identifying languages")
    (description "Liblangtag implements an interface to work with tags
for identifying languages as described in RFC 5646.  It supports the
extensions described in RFC6067 and RFC6497, and Extension T for
language/locale identifiers as described in the Unicode CLDR
standard 21.0.2.")
    (license (list license:lgpl3+ license:mpl2.0)))) ; dual license

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
    (license (license:non-copyleft "file://LICENSE"
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
    (arguments
     '(#:configure-flags '("--disable-werror")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'adjust-for-ICU-65
                    (lambda _
                      ;; Fix build with ICU 65 and later.  Taken from this
                      ;; upstream commit, remove for libfreehand > 0.1.2:
                      ;; https://gerrit.libreoffice.org/#/c/80224/
                      (substitute* "src/lib/libfreehand_utils.cpp"
                        (("U16_NEXT.*" all)
                         (string-append all ";\n")))
                      #t)))))
    (native-inputs
     (list cppunit doxygen gperf perl pkg-config))
    (inputs
     (list icu4c lcms))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     (list librevenge zlib))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libfreehand")
    (synopsis "Library for parsing the FreeHand format")
    (description "Libfreehand is a library that parses the file format of
Aldus/Macromedia/Adobe FreeHand documents.")
    (license license:mpl2.0)))

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
               "1fhkn013gzg59f4z7rldpbi0nj7lgdqzxanspsqa6axvmahw2dpg"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; This can be removed with the next release.
          ;; https://gerrit.libreoffice.org/c/libmspub/+/73814
          (substitute* "src/lib/MSPUBMetaData.h"
            (("include <vector>" all)
             (string-append all "\n#include <cstdint>")))))))
    (build-system gnu-build-system)
    (native-inputs
     (list doxygen pkg-config))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     (list icu4c librevenge zlib))
    (inputs
     (list boost))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libmspub")
    (synopsis "Library for parsing the Microsoft Publisher format")
    (description "Libmspub is a library that parses the file format of
Microsoft Publisher documents of all versions.")
    (license license:mpl2.0)))

(define-public libnumbertext
  (package
    (name "libnumbertext")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Numbertext/libnumbertext/"
                           "releases/download/" version
                           "/libnumbertext-" version ".tar.xz"))
       (sha256
        (base32 "1z31idnmd9iv6ra5dcsn7q70wv32clk2sqc5bbhimqcsp2f29f0p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (home-page "https://github.com/Numbertext/libnumbertext")
    (synopsis "Language-neutral @code{NUMBERTEXT} and @code{MONEYTEXT} functions")
    (description
     "The libnumbertext library provides language-neutral @code{NUMBERTEXT}
and @code{MONEYTEXT} functions for LibreOffice Calc, available for C++ and
Java.")
    (license (list license:lgpl3+ license:bsd-3))))

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
    (arguments
     `(#:configure-flags '("--disable-werror")))
    (native-inputs
     (list doxygen pkg-config))
    (propagated-inputs ; in Requires field of .pkg
     (list librevenge))
    (inputs
     (list boost zlib))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libpagemaker")
    (synopsis "Library for parsing the PageMaker format")
    (description "Libpagemaker is a library that parses the file format of
Aldus/Adobe PageMaker documents.  Currently it only understands documents
created by PageMaker version 6.x and 7.")
    (license license:mpl2.0)))

(define-public libvisio
  (package
    (name "libvisio")
    (version "0.1.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://dev-www.libreoffice.org/src/libvisio/"
                          "libvisio-" version ".tar.xz"))
      (sha256 (base32
               "0k7adcbbf27l7n453cca1m6s9yj6qvb5j6bsg2db09ybf3w8vbwg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list cppunit doxygen gperf perl pkg-config))
    (propagated-inputs ; in Requires or Requires.private field of .pkg
     (list icu4c librevenge libxml2))
    (inputs
     (list boost))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libvisio")
    (synopsis "Library for parsing the Microsoft Visio format")
    (description "Libvisio is a library that parses the file format of
Microsoft Visio documents of all versions.")
    (license license:mpl2.0)))

(define-public libodfgen
  (package
    (name "libodfgen")
    (version "0.1.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libwpd/libodfgen/"
                          "libodfgen-" version "/libodfgen-" version ".tar.xz"))
      (sha256 (base32
               "0986c5gw4vdfz7bcmpdfz07inba5wxsx4f6xvndknqj6zlkh082m"))))
    (build-system gnu-build-system)
    (native-inputs
     (list doxygen pkg-config))
    (propagated-inputs ; in Requires field of .pkg
     (list librevenge))
    (inputs
     (list boost libxml2 zlib))
    (arguments
     ;; Avoid triggering configure errors by simple inclusion of Boost headers.
     `(#:configure-flags '("--disable-werror")))
    (home-page "https://sourceforge.net/p/libwpd/wiki/libodfgen/")
    (synopsis "ODF (Open Document Format) library")
    (description "Libodfgen is a library for generating documents in the
Open Document Format (ODF).  It provides generator implementations for all
document interfaces supported by librevenge:
text documents, vector drawings, presentations and spreadsheets.")
    (license (list license:mpl2.0 license:lgpl2.1+)))) ; dual license

(define-public libmwaw
  (package
    (name "libmwaw")
    (version "0.3.19")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libmwaw/libmwaw/libmwaw-"
                          version "/libmwaw-" version ".tar.xz"))
      (sha256
       (base32 "1bx5xnw8sk5h26x2z7hfac7hfbm68zqg0jilp15qr0pwxqsf4wmj"))))
    (build-system gnu-build-system)
    (native-inputs
     (list doxygen pkg-config))
    (propagated-inputs                  ; in Requires field of .pc file
     (list librevenge))
    (inputs
     (list boost zlib))
    (home-page "https://sourceforge.net/p/libmwaw/wiki/Home/")
    (synopsis "Import library for some old Macintosh text documents")
    (description "Libmwaw contains some import filters for old Macintosh
text documents (MacWrite, ClarisWorks, ... ) and for some graphics and
spreadsheet documents.")
    (license (list license:mpl2.0 license:lgpl2.1+))))  ; dual license

(define-public libstaroffice
  (package
    (name "libstaroffice")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fosnola/libstaroffice/releases/download/"
                           version "/libstaroffice-" version ".tar.xz"))
       (sha256 (base32
                "1ny8411273k2bq7mnpmcvri3rd46b2j67wfypqkp3y8nhanv0kzr"))))
    (build-system gnu-build-system)
    (inputs
     (list librevenge zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/fosnola/libstaroffice")
    (synopsis "Provides LibreOffice support for old StarOffice documents")
    (description "@code{libstaroffice} is an import filter for the document formats
from the old StarOffice (.sdc, .sdw, ...).")
    (license (list license:mpl2.0 license:lgpl2.1+)))) ; dual license

(define-public libwps
  (package
    (name "libwps")
    (version "0.4.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/" name "/" name "/"
                          name "-" version "/" name "-" version ".tar.xz"))
      (sha256 (base32
               "1nsfacqp5sfkyayw7q0wp68lidksd1wjdix8qmsbf0vdl19gn6p2"))))
    (build-system gnu-build-system)
    (native-inputs
     (list doxygen pkg-config))
    (propagated-inputs                  ; in Requires field of .pkg
     (list librevenge))
    (inputs
     (list boost zlib))
    (home-page "http://libwps.sourceforge.net/")
    (synopsis "Import library for Microsoft Works text documents")
    (description "Libwps is a library for importing files in the Microsoft
Works word processor file format.")
    (license (list license:mpl2.0 license:lgpl2.1+))))  ; dual license

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
    (list boost icu4c libpng librevenge zlib))
    (native-inputs
     (list cppunit doxygen pkg-config))
    (home-page "https://wiki.documentfoundation.org/DLP/Libraries/libzmf")
    (synopsis "Parses file format of Zoner Callisto/Draw documents")
    (description "Libzmf is a library that parses the file format of Zoner
Callisto/Draw documents.  Currently it only understands documents created by
Zoner Draw version 4 and 5.")
    (license license:mpl2.0)))

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
     (list autoconf automake libtool))
    (inputs
     (list perl))
    (native-search-paths (list (search-path-specification
                                (variable "DICPATH")
                                (files '("share/hunspell")))))
    (home-page "https://hunspell.github.io/")
    (synopsis "Spell checker")
    (description "Hunspell is a spell checker and morphological analyzer
library and program designed for languages with rich morphology and complex
word compounding or character encoding.")
    ;; Triple license, including "mpl1.1 or later".
    (license (list license:mpl1.1 license:gpl2+ license:lgpl2.1+))))

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
    (native-inputs (list unzip))
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
    (license license:mpl2.0)))

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

(define-public hunspell-dict-pl
  (package
    (name "hunspell-dict-pl")
    (version "20200327")
    (source
     (origin
       (method url-fetch)
       ;; Since creators of dictionary host only the latest daily release,
       ;; we're using version mirrored by Arch Linux, which seems good
       ;; enough. They're mirroring hunspell-pl releases since 2011.
       (uri (string-append "https://sources.archlinux.org/other/community/"
                           "hunspell-pl/sjp-myspell-pl-"
                           version ".zip"))
       (sha256 (base32
                "14mzf8glxkp2775dcqisb1zv6r8ncm3bvzl46q352rwyl2dg1c59"))))

    (build-system trivial-build-system)
    (native-inputs (list unzip))
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
                          (unzip (search-input-file %build-inputs
                                                    "/bin/unzip")))
                     (invoke unzip "-j" "-o" (assoc-ref %build-inputs "source"))
                     (invoke unzip "-j" "-o" "pl_PL.zip")
                     (for-each (cut install-file <> hunspell)
                               (find-files "."
                                           ,(string-append "pl_PL"
                                                           "\\.(dic|aff)$")))
                     (mkdir-p myspell)
                     (symlink hunspell (string-append myspell "/dicts"))
                     (for-each (cut install-file <> doc)
                               (find-files "." "\\.(txt|org|md)$"))
                     #t))))
    (synopsis "Hunspell dictionary for Polish")
    (description
     "This package provides a dictionary for the Hunspell spell-checking
library.")
    (home-page "https://sjp.pl/slownik/ort/")
    (license
     (list license:gpl2 license:mpl1.1 license:cc-by4.0 license:lgpl2.1 license:asl2.0))))

(define-public hunspell-dict-de
  (package
    (name "hunspell-dict-de")
    (version "20161207")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.j3e.de/ispell/igerman98/dict/"
                           "igerman98-" version ".tar.bz2"))
       (sha256
        (base32 "1a3055hp2bc4q4nlg3gmg0147p3a1zlfnc65xiv2v9pyql1nya8p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("hunspell/de_DE.dic")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install              ;no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/hunspell/")))
               (install-file "hunspell/de_DE.aff" share)
               (install-file "hunspell/de_DE.dic" share)
               #t))))
       #:tests? #f))        ; no tests
    (native-inputs
     (list hunspell ispell perl))
    (synopsis "Hunspell dictionary for German (de_DE)")
    (description "This package provides a dictionary for the Hunspell
spell-checking library.")
    (home-page "https://www.j3e.de/ispell/igerman98/")
    (license (list license:gpl2 license:gpl3))))

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
     (list perl))
    (home-page "http://hunspell.sourceforge.net/")
    (synopsis "Hyphenation library")
    (description "Hyphen is a hyphenation library using TeX hyphenation
patterns, which are pre-processed by a perl script.")
    ;; Triple license, including "mpl1.1 or later".
    (license
     (list license:mpl1.1 license:mpl2.0 license:gpl2+ license:lgpl2.1+))))

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
     (list pkg-config))
    (inputs
     (list hunspell perl))
    (home-page "http://hunspell.sourceforge.net/")
    (synopsis "Thesaurus")
    (description "MyThes is a simple thesaurus that uses a structured text
data file and an index file with binary search to look up words and phrases
and to return information on pronunciations, meanings and synonyms.")
    (license (license:non-copyleft "file://COPYING"
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
     (list boost icu4c zlib))
    (native-inputs
     (list cppunit pkg-config))
    (propagated-inputs
     (list librevenge)) ; mentioned in Requires field
    (home-page "https://www.libreoffice.org")
    (synopsis "Library and tools for the QuarkXPress file format")
    (description "libqxp is a library and a set of tools for reading and
converting QuarkXPress file format.  It supports versions 3.1 to 4.1.")
    (license license:mpl2.0)))

;; When updating libreoffice, also make sure to update the
;; hunspell dictionaries! They use the libreoffice version.
(define-public libreoffice
  (package
    (name "libreoffice")
    (version "7.1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://download.documentfoundation.org/libreoffice/src/"
         (version-prefix version 3) "/libreoffice-" version ".tar.xz"))
       (sha256
        (base32 "1jsskhnlyra7q6d12kkc8dxq5fgrnd8grl32bdck7j9hkwv6d13m"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("cppunit" ,cppunit)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("which" ,which)
       ("ziptime" ,ziptime)))
    (inputs
     `(("bluez" ,bluez)
       ("boost" ,boost)
       ("box2d" ,box2d)
       ("clucene" ,clucene)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("firebird" ,firebird)
       ("fontconfig" ,fontconfig)
       ("fontforge" ,fontforge)
       ("gconf" ,gconf)
       ("glew" ,glew)
       ("glm" ,glm)
       ("gnupg" ,gnupg)
       ("gobject-introspection" ,gobject-introspection)
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
       ("mariadb" ,mariadb "dev")
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
       ("qrcodegen-cpp" ,qrcodegen-cpp)
       ("redland" ,redland)
       ("sane-backends" ,sane-backends)
       ("unixodbc" ,unixodbc)
       ("unzip" ,unzip)
       ("vigra" ,vigra)
       ("xdg-utils" ,xdg-utils)
       ("xmlsec" ,xmlsec-nss)
       ("zip" ,zip)
       ("dtoa"              ; needed after version 6.4.7.2.
        ,(origin
           (method url-fetch)
           (uri "https://dev-www.libreoffice.org/src/dtoa-20180411.tgz")
           (sha256
            (base32 "1d0iwy0q5sjznv23d3nbwmy0r7m1mdzlnv5pc4izddkx9xld10h0"))))))
    (arguments
     `(#:tests? #f                     ; Building the tests already fails.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'insert-external-tarballs
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "external/tarballs")
             (copy-file (assoc-ref inputs "dtoa")
                        "external/tarballs/dtoa-20180411.tgz")
             #t))
         (add-before 'configure 'prepare-src
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute*
                 (list "sysui/CustomTarget_share.mk"
                       "solenv/gbuild/gbuild.mk"
                       "solenv/gbuild/platform/unxgcc.mk")
               (("/bin/sh") (which "sh")))

             ;; Use store references for strictly necessary commands,
             ;; but not for optional tools like ‘gdb’ and ‘valgrind’.
             (for-each (lambda (command)
                         (substitute* "desktop/scripts/soffice.sh"
                           (((format #f"~a " command))
                            (format #f "~a " (which command)))))
                       (list "dirname" "grep" "uname"))

             ;; GPGME++ headers are installed in a gpgme++ subdirectory, but
             ;; configure is hardcoded to use FHS directories.
             (substitute* "configure"
               (("GPGMEPP_CFLAGS=-I/usr")
                (string-append "GPGMEPP_CFLAGS=-I"
                               (assoc-ref inputs "gpgme"))))

             ;; /usr/bin/xdg-open doesn't exist on Guix System.
             (substitute* '("shell/source/unix/exec/shellexec.cxx"
                            "shell/source/unix/misc/senddoc.sh")
               (("/usr/bin/xdg-open")
                (search-input-file inputs "/bin/xdg-open")))))
         (add-after 'install 'reset-zip-timestamps
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (file)
                           (invoke "ziptime" file))
                         ;; So many different extensions for .zip files.
                         (find-files out "\\.(bau|dat|otp|ott|zip)$")))))
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
        "--with-vendor=GNU Guix"
        ;; Avoid using all cpu cores by default
        (format #f "--with-parallelism=~d" (parallel-job-count))
        "--disable-fetch-external"      ; disable downloads
        "--with-system-libs"            ; enable all --with-system-* flags
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
        "--disable-coinmp"
        "--disable-skia"
        ;; This could (Debian does this) be a separate output containing only
        ;; program/libfirebird_sdbclo.so, if there's a way to point to it.
        "--enable-firebird-sdbc"
        ;; XXX: PDFium support requires fetching an external tarball and
        ;; patching the build scripts to work with GCC5.  Try enabling this
        ;; when our default compiler is >=GCC 6.
        "--disable-pdfium"
        "--without-doxygen"
        "--enable-build-opensymbol")))
    (home-page "https://www.libreoffice.org/")
    (synopsis "Office suite")
    (description "LibreOffice is a comprehensive office suite.  It contains
a number of components: Writer, a word processor; Calc, a spreadsheet
application; Impress, a presentation engine; Draw, a drawing and
flowcharting application; Base, a database and database frontend;
Math for editing mathematics.")
    (license license:mpl2.0)))
