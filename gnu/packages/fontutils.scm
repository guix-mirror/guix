;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages fontutils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public freetype
  (package
   (name "freetype")
   (version "2.9.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://savannah/freetype/freetype-"
                                version ".tar.bz2"))
            (sha256 (base32
                     "0kg8w6qyiizlyzh4a8lpzslipcbv96hcg3rqqpnxba8ffbm8g3fv"))))
   (build-system gnu-build-system)
   (arguments
    ;; The use of "freetype-config" is deprecated, but other packages still
    ;; depend on it.
    `(#:configure-flags (list "--enable-freetype-config")))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (propagated-inputs
    ;; These are all in the Requires.private field of freetype2.pc.
    ;; XXX: add harfbuzz.
    `(("libpng" ,libpng)
      ("zlib" ,zlib)))
   (synopsis "Font rendering library")
   (description
    "Freetype is a library that can be used by applications to access the
contents of font files.  It provides a uniform interface to access font files.
It supports both bitmap and scalable formats, including TrueType, OpenType,
Type1, CID, CFF, Windows FON/FNT, X11 PCF, and others.  It supports high-speed
anti-aliased glyph bitmap generation with 256 gray levels.")
   (license license:freetype)           ; some files have other licenses
   (home-page "https://www.freetype.org/")))

(define-public ttfautohint
  (package
    (name "ttfautohint")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/freetype/ttfautohint-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1lgghck46p33z3hg8dnl76jryig4fh6d8rhzms837zp7x4hyfkv4"))
       (patches (list (search-patch "ttfautohint-source-date-epoch.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)))
    (arguments
     `(#:configure-flags '("--with-qt=no"))) ;no gui
    (synopsis "Automated font hinting")
    (description
     "ttfautohint provides a 99% automated hinting process and a platform for
finely hand-hinting the last 1%.  It is ideal for web fonts and supports many
scripts.")
    (license (list license:gpl2+ license:freetype)) ;choose one or the other
    (home-page "http://www.freetype.org/ttfautohint/")))

(define-public woff-tools
  (package
    (name "woff-tools")
    (version "2009.10.04")
    (source
     (origin
       (method url-fetch)
       ;; Upstream source is unversioned, so use Debian's versioned tarball
       (uri (string-append "mirror://debian/pool/main/w/woff-tools/"
                           "woff-tools_" version ".orig.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1i97gkqa6jfzlslsngqf556kx60knlgf7yc9pzsq2pizc6f0d4zl"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (arguments
     `(#:make-flags '("CC=gcc")
       #:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configuration
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "sfnt2woff" bin)
               (install-file "woff2sfnt" bin)))))))
    (synopsis "Convert between OpenType and WOFF fonts")
    (description
     "This package provides two tools:
@table @code
@item sfnt2woff
Converts OpenType fonts to WOFF fonts
@item woff2sfnt
Converts WOFF fonts to OpenType fonts
@end table")
    (license (list license:mpl1.1 license:gpl2+ license:lgpl2.1+))
    (home-page "https://people.mozilla.com/~jkew/woff/")))

(define-public ttf2eot
  (package
    (name "ttf2eot")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wget/ttf2eot.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l2yh2ialx7135pjzhjs204kk3br7zxjr09zwaia493by2adzigr"))
       (patches (list (search-patch "ttf2eot-cstddef.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configuration
         (replace 'install              ; no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "ttf2eot" bin)
               #t))))))
    (synopsis "Convert from TrueType to Embeddable Open Type")
    (description
     "This package contains a commandline wrapper around OpenTypeUtilities.cpp
from Chromium, used to make EOT (Embeddable Open Type) files from
TTF (TrueType/OpenType Font) files.")
    ;; While the README states "License: Derived from WebKit, so BSD/LGPL
    ;; 2/LGPL 2.1", the single derived source file includes only BSD in its
    ;; license header, and the wrapper source contains no license header.
    (license license:bsd-2)
    (home-page "https://github.com/wget/ttf2eot")))

(define-public ttf2pt1
  (package
    (name "ttf2pt1")
    (version "3.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ttf2pt1/ttf2pt1/"
                                  version "/ttf2pt1-" version ".tgz"))
              (sha256
               (base32
                "1l718n4k4widx49xz7qrj4mybzb8q67kp2jw7f47604ips4654mf"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove trailing backslashes in the sed expression of the
                  ;; 'install' rule since sed would otherwise fail.
                  (substitute* "Makefile"
                    (("\\|;\\\\[[:space:]]*$") "|; "))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                                ;no tests
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Makefile"
                          (("INSTDIR =.*")
                           (string-append "INSTDIR = " out "\n"))
                          (("OWNER = .*")
                           "OWNER = `id -un`\n")
                          (("GROUP = .*")
                           "GROUP = `id -g`\n"))
                        #t)))
                  (replace 'build
                    (lambda _
                      (invoke "make" "-j"
                              (number->string (parallel-job-count))
                              "all" "CC=gcc"))))))
    (inputs `(("perl" ,perl)))
    (synopsis "Convert TrueType fonts to Postscript Type 1")
    (description
     "TTF2PT1 provides tools to convert most TrueType fonts (or other formats
supported by the FreeType library) to an Adobe Type 1 @file{.pfa} or
@file{.pfb} file.  Another use is as a hinting engine: feed it an unhinted or
poorly hinted Adobe Type 1 font through the FreeType library and get it back
with freshly generated hints.  The files produced by default are in
human-readable form, which further needs to be encoded with t1utilities to
work with most software requiring Type 1 fonts.")
    (home-page "http://ttf2pt1.sourceforge.net/")
    (license license:bsd-3)))

(define-public woff2
  (let ((commit "4e698b8c6c5e070d53c340db9ddf160e21070ede")
        (revision "1"))
    (package
      (name "woff2")
      (version (string-append "20160306-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/woff2.git")
                      (commit commit)))
                (file-name (string-append name "-" version ".tar.xz"))
                (sha256
                 (base32
                  "0wka0yhf0cjmd4rv2jckxpyv6lb5ckj4nj0k1ajq5hrjy7f30lcp"))
                (patches (list (search-patch "woff2-libbrotli.patch")))))
      (build-system gnu-build-system)
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("brotli" ,brotli)))
      (arguments
       `(#:tests? #f                    ;no tests
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (bin (string-append out "/bin")))
                          (install-file "woff2_compress" bin)
                          (install-file "woff2_decompress" bin)
                          #t))))))
      (synopsis "Compress TrueType fonts to WOFF2")
      (description
       "This package provides utilities for compressing/decompressing TrueType
fonts to/from the WOFF2 format.")
      (license license:asl2.0)
      (home-page "https://github.com/google/woff2"))))

(define-public fontconfig
  (package
   (name "fontconfig")
   (version "2.13.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "https://www.freedesktop.org/software/fontconfig/release/fontconfig-"
                   version ".tar.bz2"))
            (sha256 (base32
                     "0hb700a68kk0ip51wdlnjjc682kvlrmb6q920mzajykdk0mdsmgn"))))
   (build-system gnu-build-system)
   ;; In Requires or Requires.private of fontconfig.pc.
   (propagated-inputs `(("expat" ,expat)
                        ("freetype" ,freetype)
                        ("libuuid" ,util-linux)))
   (inputs `(("gs-fonts" ,gs-fonts)))
   (native-inputs
    `(("gperf" ,gperf)
      ("pkg-config" ,pkg-config)))
   (arguments
    `(#:configure-flags
      (list "--with-cache-dir=/var/cache/fontconfig"
            ;; register gs-fonts as default fonts
            (string-append "--with-default-fonts="
                           (assoc-ref %build-inputs "gs-fonts")
                           "/share/fonts")

            ;; Register fonts from user and system profiles.
            (string-append "--with-add-fonts="
                           "~/.guix-profile/share/fonts,"
                           "/run/current-system/profile/share/fonts")

            ;; python is not actually needed
            "PYTHON=false")
      #:phases
      (modify-phases %standard-phases
        (replace 'install
                 (lambda _
                   ;; Don't try to create /var/cache/fontconfig.
                   (invoke "make" "install"
                           "fc_cachedir=$(TMPDIR)"
                           "RUN_FC_CACHE_TEST=false"))))))
   (synopsis "Library for configuring and customizing font access")
   (description
    "Fontconfig can discover new fonts when installed automatically;
perform font name substitution, so that appropriate alternative fonts can
be selected if fonts are missing;
identify the set of fonts required to completely cover a set of languages;
have GUI configuration tools built as it uses an XML-based configuration file;
efficiently and quickly find needed fonts among the set of installed fonts;
be used in concert with the X Render Extension and FreeType to implement
high quality, anti-aliased and subpixel rendered text on a display.")
   ; The exact license is more X11-style than BSD-style.
   (license (license:non-copyleft "file://COPYING"
                       "See COPYING in the distribution."))
   (home-page "https://www.freedesktop.org/wiki/Software/fontconfig")))

(define-public t1lib
  (package
   (name "t1lib")
   (version "5.1.2")
   (source (origin
            (method url-fetch)
            (uri (list (string-append "ftp://sunsite.unc.edu/pub/Linux/libs/"
                                      "graphics/" name "-" version ".tar.gz")
                       (string-append "https://fossies.org/linux/misc/old/"
                                      name "-" version ".tar.gz")))
            (sha256 (base32
                     "0nbvjpnmcznib1nlgg8xckrmsw3haa154byds2h90y2g0nsjh4w2"))
            (patches (search-patches
                       "t1lib-CVE-2010-2642.patch" ; 2011-0443, 2011-5244
                       "t1lib-CVE-2011-0764.patch"
                       "t1lib-CVE-2011-1552+.patch")))) ; 2011-1553, 2011-1554
   (properties `((lint-hidden-cve . ("CVE-2011-0433"
                                     "CVE-2011-1553"
                                     "CVE-2011-1554"
                                     "CVE-2011-5244"))))
   (build-system gnu-build-system)
   (arguments
    ;; Making the documentation requires latex, but t1lib is also an input
    ;; for building texlive.
    `(#:tests? #f ; no test target
      #:make-flags
      '("without_doc")))
   (synopsis "Library for generating bitmaps from Type 1 fonts")
   (description
    "T1lib is a library for generating/rasterising bitmaps from Type 1 fonts.
It is based on the code of the X11 rasteriser of the X11 project.

The bitmaps created by t1lib are returned in a data structure with type
GLYPH.  This special GLYPH-type is also used in the X11 window system to
describe character bitmaps.  It contains the bitmap data as well as some
metric information.  But t1lib is in itself entirely independent of the
X11-system or any other graphical user interface.")
   (license license:gpl2)
   (home-page "http://www.t1lib.org/")))

(define-public teckit
  (package
    (name "teckit")
    (version "2.5.9")                   ;signed by key 0xC9183BEA0288CDEE
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/silnrsi/teckit/releases/"
                           "download/v" version "/teckit-" version ".tar.gz"))
       (sha256
        (base32 "0gbxyip4wdibirdg2pvzayzyy927vxyd6dfyfiflx8zg88qzn8v8"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("expat" ,expat)))
    (native-inputs
     `(("perl" ,perl)))                 ;for the tests
    (synopsis "Toolkit for encoding conversions")
    (description
     "TECkit is a low-level toolkit intended to be used by other applications
that need to perform encoding conversions (e.g., when importing legacy data
into a Unicode-based application).  The primary component of the TECkit
package is therefore a library that performs conversions; this is the
\"TECkit engine\".  The engine relies on mapping tables in a specific binary
format (for which documentation is available); there is a compiler that
creates such tables from a human-readable mapping description (a simple
text file).

To facilitate the development and testing of mapping tables for TECkit,
several applications are also included in the current package; these
include simple tools for applying conversions to plain-text and Standard
Format files, as well as both command-line and simple GUI versions of the
TECkit compiler.  However, it is not intended that these tools will be the
primary means by which end users perform conversions, and they have not
been designed, tested, and debugged to the extent that general-purpose
applications should be.")
    (license license:lgpl2.1+)
    (home-page "http://scripts.sil.org/cms/scripts/page.php?cat_id=teckit")))

(define-public graphite2
  (package
   (name "graphite2")
   (version "1.3.13")
   (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/silnrsi/graphite/releases/"
                           "download/" version "/" name "-" version ".tgz"))
       (sha256
        (base32
         "01jzhwnj1c3d68dmw15jdxly0hwkmd8ja4kw755rbkykn1ly2qyx"))))
   (build-system cmake-build-system)
   (native-inputs
    `(("python" ,python-2) ; because of "import imap" in tests
      ("python-fonttools" ,python2-fonttools)))
   (inputs
    `(("freetype" ,freetype)))
   (synopsis "Reimplementation of the SIL Graphite text processing engine")
   (description
    "Graphite2 is a reimplementation of the SIL Graphite text processing
engine.  Graphite is a smart font technology designed to facilitate the
process known as shaping.  This process takes an input Unicode text string
and returns a sequence of positioned glyphids from the font.")
   (license license:lgpl2.1+)
   (home-page "https://github.com/silnrsi/graphite")))

(define-public potrace
  (package
    (name "potrace")
    (version "1.15")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/potrace/" version
                          "/potrace-" version ".tar.gz"))
      (sha256
       (base32
        "17ajildjp14shsy339xarh1lw1p0k60la08ahl638a73mh23kcx9"))
      (patches (search-patches "potrace-tests.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("ghostscript" ,ghostscript))) ;for tests
    (inputs `(("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
      `("--with-libpotrace"))) ; install library and headers
    (synopsis "Transform bitmaps into vector graphics")
    (description
     "Potrace is a tool for tracing a bitmap, which means, transforming a
bitmap into a smooth, scalable image.  The input is a bitmap (PBM, PGM, PPM,
or BMP format), and the default output is an encapsulated PostScript
file (EPS).  A typical use is to create EPS files from scanned data, such as
company or university logos, handwritten notes, etc.  The resulting image is
not \"jaggy\" like a bitmap, but smooth.  It can then be rendered at any
resolution.")
    (license license:gpl2+)
    (home-page "http://potrace.sourceforge.net/")))

(define-public libotf
  (package
    (name "libotf")
    (version "0.9.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/m17n/libotf-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0sq6g3xaxw388akws6qrllp3kp2sxgk2dv4j79k6mm52rnihrnv8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("freetype" ,freetype)))
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Library for handling OpenType Font")
    (description "This library can read Open Type Layout Tables from an OTF
file.  Currently these tables are supported; head, name, cmap, GDEF, GSUB, and
GPOS.  It can convert a Unicode character sequence to a glyph code sequence by
using the above tables.")
    (license license:lgpl2.0+)))

(define-public libspiro
  (package
    (name "libspiro")
    (version "0.5.20150702")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/fontforge/libspiro/releases"
                          "/download/" version "/libspiro-dist-" version ".tar.gz"))
      (sha256
       (base32
        "153ckwj6h3wwlsgcppzqj8cymv1927hi8ar8fzpchq5q89cj2kai"))))
    (build-system gnu-build-system)
    (synopsis "Clothoid to bezier conversion library")
    (description
     "Raph Levien's Spiro package as a library.  A mechanism for drawing
smooth contours with constant curvature at the spline joins.")
    (license license:gpl2+)
    (home-page "http://libspiro.sourceforge.net/")))

(define-public libuninameslist
  (package
    (name "libuninameslist")
    (version "20190305")
    (home-page "https://github.com/fontforge/libuninameslist")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/releases/download/" version
                           "/libuninameslist-dist-" version ".tar.gz"))
       (sha256
        (base32
         "1rwd2bgcyvign9agyjsr3v2fr9j1cg2wi6g0z2wwg1az32scknwq"))))
    (build-system gnu-build-system)
    (synopsis "Unicode names and annotation list")
    (description
     "LibUniNamesList holds www.unicode.org Nameslist.txt data which can be
useful for programs that need Unicode \"Names\", \"Annotations\", and block
definitions.")
    ;; COPYING specifies GPL2, but according to LICENSE it only covers the
    ;; configure script.  The actual code is BSD-3, and the Unicode data
    ;; is governed by an X11-style license only found on the web.
    (license (list license:bsd-3
                   (license:x11-style
                    "https://www.unicode.org/copyright.html#License")))))

(define-public fontforge
  (package
   (name "fontforge")
   (version "20190317")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/fontforge/fontforge/releases/download/"
                  version "/fontforge-" version ".tar.gz"))
            (sha256 (base32
                     "1ddqbpc32cgbccdnv0lfw0qhj59hcqzb7616ph5lkvm91pnas4dp"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs `(("cairo"           ,cairo)
             ("fontconfig"      ,fontconfig) ;dlopen'd
             ("freetype"        ,freetype)
             ("gettext"         ,gettext-minimal)
             ("glib"            ,glib) ;needed for pango detection
             ("libICE"          ,libice)
             ("libSM"           ,libsm)
             ("libX11"          ,libx11)
             ("libXi"           ,libxi)
             ("libjpeg"         ,libjpeg)
             ("libltdl"         ,libltdl)
             ("libpng"          ,libpng)
             ("libspiro"        ,libspiro)
             ("libtiff"         ,libtiff)
             ("libungif"        ,libungif)
             ("libuninameslist" ,libuninameslist)
             ("libxft"          ,libxft)
             ("libxml2"         ,libxml2)
             ("pango"           ,pango)
             ("potrace"         ,potrace)
             ;; FIXME: We use Python 2 here because there is a bug in Python
             ;; 3.7 that is triggered when Py_Main is called after Py_Init, as
             ;; is done by fontforge.  This will be fixed in Python 3.7.1.
             ("python"          ,python-2)
             ("zlib"            ,zlib)))
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'install 'set-library-path
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (potrace (string-append (assoc-ref inputs "potrace") "/bin")))
              (wrap-program (string-append out "/bin/fontforge")
                ;; Fontforge dynamically opens libraries.
                `("LD_LIBRARY_PATH" ":" prefix
                  ,(map (lambda (input)
                          (string-append (assoc-ref inputs input)
                                         "/lib"))
                        '("libtiff" "libjpeg" "libpng" "libungif"
                          "libxml2" "zlib" "libspiro" "freetype"
                          "pango" "cairo" "fontconfig")))
                ;; Checks for potrace program at runtime
                `("PATH" ":" prefix (,potrace)))
              #t))))))
   (synopsis "Outline font editor")
   (description
    "FontForge allows you to create and modify postscript, truetype and
opentype fonts.  You can save fonts in many different outline formats, and
generate bitmaps.")
   (license license:gpl3+)
   (home-page "https://fontforge.github.io/en-US/")))

(define-public python2-ufolib
  (package
    (name "python2-ufolib")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ufoLib" version ".zip"))
       (sha256
        (base32 "07qy6mx7z0wi9a30lc2hj5i9q1gnz1n8l40dmjz2c19mj9s6mz9l"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python2-fonttools" ,python2-fonttools)))
    (native-inputs
     `(("unzip" ,unzip)
       ("python2-pytest" ,python2-pytest)
       ("python2-pytest-runner" ,python2-pytest-runner)))
    (home-page "https://github.com/unified-font-object/ufoLib")
    (synopsis "Low-level UFO reader and writer")
    (description
     "UfoLib reads and writes Unified Font Object (UFO)
files.  UFO is a file format that stores fonts source files.")
    (license license:bsd-3)))

(define-public python2-defcon
  (package
    (name "python2-defcon")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "defcon" version ".zip"))
       (sha256
        (base32
         "03jlm2gy9lvbwj68kfdm43yaddwd634jwkdg4wf0jxx2s8mwbg22"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (native-inputs
     `(("unzip" ,unzip)
       ("python2-pytest" ,python2-pytest)
       ("python2-pytest-runner" ,python2-pytest-runner)))
    (propagated-inputs
     `(("python2-fonttools" ,python2-fonttools)
       ("python2-ufolib" ,python2-ufolib)))
    (home-page "https://pypi.python.org/pypi/defcon")
    (synopsis "Flexible objects for representing @acronym{UFO, unified font object} data")
    (description
     "Defcon is a set of @acronym{UFO, unified font object} based objects
optimized for use in font editing applications.  The objects are built to
be lightweight, fast and flexible.  The objects are very bare-bones and
they are not meant to be end-all, be-all objects.  Rather, they are meant
to provide base functionality so that you can focus on your application’s
behavior, not object observing or maintaining cached data.  Defcon
implements UFO3 as described by the UFO font format.")
    (license license:expat)))

(define-public nototools
  (package
    (name "nototools")
    (version "20170925")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/googlei18n/nototools/"
                           "archive/v2017-09-25-tooling-for-phase3-"
                           "update.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pvacw18cm9l4sb66pqyjc7hc74xhhfxc7kd5ald8lixf4wzg0s8"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python2-booleanoperations" ,python2-booleanoperations)
       ("python2-defcon" ,python2-defcon)
       ("python2-fonttools" ,python2-fonttools)
       ("python2-pillow" ,python2-pillow)
       ("python2-pyclipper" ,python2-pyclipper)
       ("python2-ufolib" ,python2-ufolib)))
    (home-page "https://github.com/googlei18n/nototools")
    (synopsis "Noto fonts support tools and scripts")
    (description
     "Nototools is a Python package containing Python scripts used to
maintain the Noto Fonts project.")
    (license (list license:asl2.0
                   ;; Sample texts are attributed to UN and OHCHR.
                   ;; The permissions on the UDHR are pretty lax:
                   ;; http://www.ohchr.org/EN/UDHR/Pages/Introduction.aspx
                   ;; "If UDHR translations or materials are reproduced, users
                   ;; should make reference to this website as a source by
                   ;; providing a link."
                   license:public-domain
                   (license:non-copyleft
                    "file://sample_texts/attributions.txt"
                    "See sample_texts/attributions.txt in the distribution.")))))
