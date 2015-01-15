;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages python)
  #:use-module (gnu packages image)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))

(define-public freetype
  (package
   (name "freetype")
   (version "2.4.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://savannah/freetype/freetype-"
                                version ".tar.gz"))
            (sha256 (base32
                     "0gpcz6swir64kp0dk3rwgqqkmf48b90dqgczdmznjjryhrahx9r9"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
       (alist-replace
        'install
        (lambda* (#:key outputs #:allow-other-keys #:rest args)
         (let ((install (assoc-ref %standard-phases 'install))
               (include (string-append (assoc-ref outputs "out") "/include")))
           (apply install args)
           ;; Unravel one directory, since ft2build.h includes directly from
           ;; freetype/, not freetype2/freetype; this is announced in the file
           ;; to be changed in a future release.
           (symlink (string-append include "/freetype2/freetype")
                    (string-append include "/freetype"))))
       %standard-phases)))
   (synopsis "Font rendering library")
   (description
    "Freetype is a library that can be used by applications to access the
contents of font files.  It provides a uniform interface to access font files.
It supports both bitmap and scalable formats, including TrueType, OpenType,
Type1, CID, CFF, Windows FON/FNT, X11 PCF, and others.  It supports high-speed
anti-aliased glyph bitmap generation with 256 gray levels.")
   (license license:freetype)           ; some files have other licenses
   (home-page "http://www.freetype.org/")))

(define-public fontconfig
  (package
   (name "fontconfig")
   (version "2.10.93")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://www.freedesktop.org/software/fontconfig/release/fontconfig-"
                   version ".tar.bz2"))
            (sha256 (base32
                     "172j5vsgx2xplsk5mrxrspbn5lrswq6gnxkxjgcrx0j8i0kiz47a"))))
   (build-system gnu-build-system)
   (inputs `(("expat" ,expat)
             ("freetype" ,freetype)
             ("gs-fonts" ,gs-fonts)))
   (native-inputs
      `(("pkg-config" ,pkg-config)))
   (arguments
     `(#:configure-flags
               ;; point to user profile instead of /usr/share/fonts in /etc/fonts.conf
        (list "--with-default-fonts=~/.guix-profile/share/fonts"
              ;; register gs-fonts
              (string-append "--with-add-fonts="
                             (assoc-ref %build-inputs "gs-fonts")
                             "/share/fonts"))))
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
   (license (license:bsd-style "file://COPYING"
                       "See COPYING in the distribution."))
   (home-page "http://www.freedesktop.org/wiki/Software/fontconfig")))

(define-public t1lib
  (package
   (name "t1lib")
   (version "5.1.2")
   (source (origin
            (method url-fetch)
            (uri "ftp://sunsite.unc.edu/pub/Linux/libs/graphics/t1lib-5.1.2.tar.gz")
            (sha256 (base32
                     "0nbvjpnmcznib1nlgg8xckrmsw3haa154byds2h90y2g0nsjh4w2"))))
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
   (version "2.5.1")
   (source (origin
            (method url-fetch)
            (uri (list
                  (string-append
                   "http://scripts.sil.org/svn-view/teckit/TAGS/TECkit_"
                   (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                   ".tar.gz")
                  "http://pkgs.fedoraproject.org/repo/pkgs/teckit/TECkit_2_5_1.tar.gz/4913f71f0f42bfd9cf8f161688b35dea/TECkit_2_5_1.tar.gz"
                  ;; This used to be the canonical URL but it vanished.
                  ;; See <http://bugs.gnu.org/19600>.
                  ;; (string-append
                  ;;  "http://scripts.sil.org/svn-view/teckit/TAGS/TECkit_"
                  ;;  (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                  ;;  ".tar.gz")
                  ))
            (sha256 (base32
                     "0fjiwvic8mdxpkyccfp7zh26y9xnvkp0skqbyfkrjiacd191k82r"))
            (patches (list (search-patch "teckit-cstdio.patch")))))
   (build-system gnu-build-system)
   (inputs `(("zlib" ,zlib)))
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
   (version "1.2.4")
   (source
     (origin
       (method url-fetch)
       (uri (string-append
              "mirror://sourceforge/silgraphite/graphite2/graphite2-"
              version ".tgz"))
       (sha256
         (base32
           "00xhv1mp640fr3wmdzwn4yz0g56jd4r9fb7b02mc1g19h0bdbhsb"))))
   (build-system cmake-build-system)
   (inputs
    `(("freetype" ,freetype)
      ("perl" ,perl)
      ("python" ,python-2))) ; because of "import imap" in tests
   (synopsis "Reimplementation of the SIL Graphite text processing engine")
   (description
    "Graphite2 is a reimplementation of the SIL Graphite text processing
engine.  Graphite is a smart font technology designed to facilitate the
process known as shaping.  This process takes an input Unicode text string
and returns a sequence of positioned glyphids from the font.")
   (license license:lgpl2.1+)
   (home-page "http://projects.palaso.org/projects/graphitedev")))

(define-public potrace
  (package
    (name "potrace")
    (version "1.11")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/potrace/potrace-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1bbyl7jgigawmwc8r14znv8lb6lrcxh8zpvynrl6s800dr4yp9as"))))
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

(define-public libspiro
  (package
    (name "libspiro")
    (version "20071029")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libspiro/libspiro/"
                          version "/libspiro_src-" version ".tar.bz2"))
      (sha256
       (base32
        "1kylz8pvwnb85yya150r9i6mhbpzx38f32qy523qg3ylgd9b3zhy"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f))          ;no tests
    (synopsis "Clothoid to bezier conversion library")
    (description
     "Raph Levien's Spiro package as a library.  A mechanism for drawing
smooth contours with constant curvature at the spline joins.")
    (license license:gpl2+)
    (home-page "http://libspiro.sourceforge.net/")))

(define-public libuninameslist
  (package
    (name "libuninameslist")
    (version "0.4.20140731")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fontforge/libuninameslist/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "016zxffpc8iwpxxmnjkdirn6dsbcvdb2wjdrp123sf79f4nsynyj"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool"  ,libtool)
                     ("libtool-bin" ,libtool "bin")))
    (arguments
     `(#:phases (alist-cons-before
                 'configure 'bootstrap
                 (lambda _
                   (zero? (system* "autoreconf" "-vi")))
                 %standard-phases)))
    (synopsis "Unicode names and annotation list")
    (description
     "LibUniNamesList holds www.unicode.org Nameslist.txt data which can be
useful for programs that need Unicode \"Names\", \"Annotations\", and block
definitions.")
    (license license:gpl2)
    (home-page "https://github.com/fontforge/libuninameslist")))

(define-public fontforge
  (package
   (name "fontforge")
   (version "20120731-b")               ;aka 1.0
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/fontforge/fontforge_full-"
                                version ".tar.bz2"))
            (sha256 (base32
                     "1dhg0i2pf76j40cb9g1wzpag21fgarpjaad0hdbk27i1zz588q8v"))))
   (build-system gnu-build-system)
   ;; TODO: Add python for scripting support.
   (inputs `(("gettext"         ,gnu-gettext)
             ("libtiff"         ,libtiff)
             ("libjpeg"         ,libjpeg)
             ("libpng"          ,libpng)
             ("giflib"          ,giflib) ;needs giflib 4.*
             ("libxml2"         ,libxml2)
             ("libX11"          ,libx11)
             ("libXi"           ,libxi)
             ("libICE"          ,libice)
             ("libSM"           ,libsm)
             ("freetype"        ,freetype)
             ("potrace"         ,potrace)
             ("libspiro"        ,libspiro)
             ("zlib"            ,zlib)
             ("cairo"           ,cairo)
             ("fontconfig"      ,fontconfig) ;dlopen'd
             ("libuninameslist" ,libuninameslist)
             ("pango"           ,pango)
             ("glib"            ,glib))) ;needed for pango detection
   (arguments
    '(#:configure-flags `("--enable-double")
      #:tests? #f
      #:phases
      (alist-cons-before
       'configure 'patch-configure
       (lambda* (#:key inputs #:allow-other-keys)
         (let ((libxml2 (assoc-ref inputs "libxml2"))
               (cairo   (assoc-ref inputs "cairo"))
               (pango   (assoc-ref inputs "pango")))
           (substitute* "configure"
             ;; configure looks for a directory to be present to determine
             ;; whether libxml2 is available, rather than checking for the
             ;; library or headers.  Point it to the correct directory.
             (("/usr/include/libxml2")
              (string-append libxml2 "/include/libxml2"))
             ;; Similary, the search directories for cairo and pango are
             ;; hard-coded.
             (("gww_prefix in.*") (string-append "gww_prefix in "
                                                 cairo " " pango "\n")))))
       (alist-cons-after
        'install 'set-library-path
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out"))
                (potrace (string-append (assoc-ref inputs "potrace") "/bin")))
            (wrap-program (string-append out "/bin/fontforge")
                          ;; Fontforge dynamically opens libraries.
                          `("LD_LIBRARY_PATH" ":" prefix
                            ,(map (lambda (input)
                                    (string-append (assoc-ref inputs input)
                                                   "/lib"))
                                  '("libtiff" "libjpeg" "libpng" "giflib"
                                    "libxml2" "zlib" "libspiro" "freetype"
                                    "pango" "cairo" "fontconfig")))
                          ;; Checks for potrace program at runtime
                          `("PATH" ":" prefix (,potrace)))))
        %standard-phases))))
   (synopsis "Outline font editor")
   (description
    "FontForge allows you to create and modify postscript, truetype and
opentype fonts.  You can save fonts in many different outline formats, and
generate bitmaps.")
   (license license:bsd-3)
   (home-page "http://fontforge.org/")))
