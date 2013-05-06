;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
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
   (synopsis "Freetype, a library to render fonts")
   (description
    "Freetype is a library that can be used by applications to access the
contents of font files. It provides a uniform interface to access font files.
It supports both bitmap and scalable formats, including TrueType, OpenType,
Type1, CID, CFF, Windows FON/FNT, X11 PCF, and others. It supports high-speed
anti-aliased glyph bitmap generation with 256 gray levels.")
   (license license:freetype)           ; some files have other licenses
   (home-page "http://www.freetype.org/")))

(define-public fontconfig
  (package
   (name "fontconfig")
   (version "2.10.91")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://www.freedesktop.org/software/fontconfig/release/fontconfig-"
                   version ".tar.bz2"))
            (sha256 (base32
                     "1vk37q3zj8bjppj3l0pkby1psialpwl263jqf6pbih2hx5a7jwm4"))))
   (build-system gnu-build-system)
   (inputs `(("expat" ,expat)
             ("freetype" ,freetype)
             ("pkg-config" ,pkg-config)))
   (synopsis "Fontconfig, a library for configuring and customising font access.")
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
   (synopsis "T1lib, a library for generating bitmaps from type 1 fonts.")
   (description
    "T1lib is a library for generating/rasterising bitmaps from Type 1 fonts.
It is based on the code of the X11 rasteriser of the X11 project.

The bitmaps created by t1lib are returned in a data structure with type
GLYPH. This special GLYPH-type is also used in the X11 window system to
describe character bitmaps. It contains the bitmap data as well as some
metric information. But t1lib is in itself entirely independent of the
X11-system or any other graphical user interface. ")
   (license license:gpl2)
   (home-page "http://www.t1lib.org/")))

(define-public teckit
  (package
   (name "teckit")
   (version "2.5.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://scripts.sil.org/svn-view/teckit/TAGS/TECkit_"
                   (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                   ".tar.gz"))
            (sha256 (base32
                     "0fjiwvic8mdxpkyccfp7zh26y9xnvkp0skqbyfkrjiacd191k82r"))))
   (build-system gnu-build-system)
   (inputs `(("patch/teckit" ,(search-patch "teckit-cstdio.patch"))
             ("zlib" ,zlib)))
   (arguments
    `(#:patches (list (assoc-ref %build-inputs "patch/teckit"))))
   (synopsis "TECkit, a toolkit for encoding conversions")
   (description
    "TECkit is a low-level toolkit intended to be used by other applications
that need to perform encoding conversions (e.g., when importing legacy data
into a Unicode-based application). The primary component of the TECkit
package is therefore a library that performs conversions; this is the
\"TECkit engine\". The engine relies on mapping tables in a specific binary
format (for which documentation is available); there is a compiler that
creates such tables from a human-readable mapping description (a simple
text file).

To facilitate the development and testing of mapping tables for TECkit,
several applications are also included in the current package; these
include simple tools for applying conversions to plain-text and Standard
Format files, as well as both command-line and simple GUI versions of the
TECkit compiler. However, it is not intended that these tools will be the
primary means by which end users perform conversions, and they have not
been designed, tested, and debugged to the extent that general-purpose
applications should be.")
   (license license:lgpl2.1+)
   (home-page "http://scripts.sil.org/cms/scripts/page.php?cat_id=teckit")))
