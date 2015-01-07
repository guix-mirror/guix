;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2014 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages image)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages ghostscript)         ;lcms
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages zip)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (srfi srfi-1))

(define-public libpng
  (package
   (name "libpng")
   (version "1.5.17")
   (source (origin
            (method url-fetch)

            ;; Note: upstream removes older tarballs.
            (uri (list (string-append "mirror://sourceforge/libpng/libpng15/"
                                      version "/libpng-" version ".tar.xz")
                       (string-append
                        "ftp://ftp.simplesystems.org/pub/libpng/png/src"
                        "/libpng15/libpng-" version ".tar.xz")))
            (sha256
             (base32 "19wj293r4plbfgb43yhrc2qx8bsch9gbazazfqrj9haa7lsk29jp"))))
   (build-system gnu-build-system)

   ;; libpng.la says "-lz", so propagate it.
   (propagated-inputs `(("zlib" ,zlib)))

   (synopsis "Library for handling PNG files")
   (description
    "Libpng is the official PNG (Portable Network Graphics) reference
library.  It supports almost all PNG features and is extensible.")
   (license license:zlib)
   (home-page "http://www.libpng.org/pub/png/libpng.html")))

(define-public libjpeg
  (package
   (name "libjpeg")
   (version "9a")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.ijg.org/files/jpegsrc.v"
                   version ".tar.gz"))
            (sha256 (base32
                     "19q5zr4n60sjcvfbyv06n4pcl1mai3ipvnd2akflayciinj3wx9s"))))
   (build-system gnu-build-system)
   (synopsis "Library for handling JPEG files")
   (description
    "Libjpeg implements JPEG image encoding, decoding, and transcoding.
JPEG is a standardized compression method for full-color and gray-scale
images.
The included programs provide conversion between the JPEG format and
image files in PBMPLUS PPM/PGM, GIF, BMP, and Targa file formats.")
   (license license:ijg)
   (home-page "http://www.ijg.org/")))

(define-public libjpeg-8
  (package (inherit libjpeg)
   (version "8d")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.ijg.org/files/jpegsrc.v"
                   version ".tar.gz"))
            (sha256 (base32
                     "1cz0dy05mgxqdgjf52p54yxpyy95rgl30cnazdrfmw7hfca9n0h0"))))))

(define-public libtiff
  (package
   (name "libtiff")
   (version "4.0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.remotesensing.org/pub/libtiff/tiff-"
                   version ".tar.gz"))
            (sha256 (base32
                     "0wj8d1iwk9vnpax2h29xqc2hwknxg3s0ay2d5pxkg59ihbifn6pa"))))
   (build-system gnu-build-system)
   (inputs `(("zlib" ,zlib)
             ("libjpeg-8" ,libjpeg-8)))
             ;; currently does not compile with libjpeg version 9
   (arguments
    `(#:configure-flags
      (list (string-append "--with-jpeg-include-dir="
                           (assoc-ref %build-inputs "libjpeg-8")
                           "/include"))))
   (synopsis "Library for handling TIFF files")
   (description
    "Libtiff provides support for the Tag Image File Format (TIFF), a format
used for storing image data.
Included are a library, libtiff, for reading and writing TIFF and a small
collection of tools for doing simple manipulations of TIFF images.")
   (license (license:bsd-style "file://COPYRIGHT"
                               "See COPYRIGHT in the distribution."))
   (home-page "http://www.libtiff.org/")))

(define-public libwmf
  (package
    (name "libwmf")
    (version "0.2.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/wvware/"
                            name "/" version
                            "/" name "-" version ".tar.gz"))
        (sha256
          (base32 "1y3wba4q8pl7kr51212jwrsz1x6nslsx1gsjml1x0i8549lmqd2v"))))

    (build-system gnu-build-system)
    (inputs
      `(("freetype" ,freetype)
        ("libjpeg" ,libjpeg)
        ("libpng",libpng)
        ("libxml2" ,libxml2)
        ("zlib" ,zlib)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (synopsis "Library for reading images in the Microsoft WMF format")
    (description
      "libwmf is a library for reading vector images in Microsoft's native
Windows Metafile Format (WMF) and for either (a) displaying them in, e.g., an X
window; or (b) converting them to more standard/free file formats such as, e.g.,
the W3C's XML-based Scaleable Vector Graphic (SVG) format.")
    (home-page "http://wvware.sourceforge.net/libwmf.html")

    ;; 'COPYING' is the GPLv2, but file headers say LGPLv2.0+.
    (license license:lgpl2.0+)))

(define-public jbig2dec
  (package
    (name "jbig2dec")
    (version "0.11")
    (source
      (origin
        (method url-fetch)
        (uri             ;; The link on the homepage is dead.
          (string-append "http://distfiles.gentoo.org/distfiles/" name "-"
                          version ".tar.gz"))
        (sha256
          (base32 "1ffhgmf2fqzk0h4k736pp06z7q5y4x41fg844bd6a9vgncq86bby"))
        (patches (list (search-patch "jbig2dec-ignore-testtest.patch")))))

    (build-system gnu-build-system)
    (synopsis "Decoder of the JBIG2 image compression format")
    (description
      "JBIG2 is designed for lossy or lossless encoding of 'bilevel'
(1-bit monochrome) images at moderately high resolution, and in
particular scanned paper documents.  In this domain it is very
efficient, offering compression ratios on the order of 100:1.

This is a decoder only implementation, and currently is in the alpha
stage, meaning it doesn't completely work yet.  However, it is
maintaining parity with available encoders, so it is useful for real
work.")
    (home-page "http://jbig2dec.sourceforge.net/")
    (license license:gpl2+)))

(define-public openjpeg
  (package
    (name "openjpeg")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "http://openjpeg.googlecode.com/files/" name "-"
                         version ".tar.gz"))
        (sha256
          (base32 "1n05yrmscpgksrh2kfh12h18l0lw9j03mgmvwcg3hm8m0lwgak9k"))))

    (build-system cmake-build-system)
    (arguments
      ;; Trying to run `$ make check' results in a no rule fault.
      '(#:tests? #f))
    (inputs
      `(("lcms" ,lcms)
        ("libpng" ,libpng)
        ("libtiff" ,libtiff)
        ("zlib" ,zlib)))
    (synopsis "JPEG 2000 codec")
    (description
      "The OpenJPEG library is a JPEG 2000 codec written in C.  It has
been developed in order to promote the use of JPEG 2000, the new
still-image compression standard from the Joint Photographic Experts
Group (JPEG).

In addition to the basic codec, various other features are under
development, among them the JP2 and MJ2 (Motion JPEG 2000) file formats,
an indexing tool useful for the JPIP protocol, JPWL-tools for
error-resilience, a Java-viewer for j2k-images, ...")
    (home-page "https://code.google.com/p/openjpeg/")
    (license license:bsd-2)))

(define-public giflib
  (package
    (name "giflib")
    (version "4.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/giflib/giflib-"
                                  (first (string-split version #\.))
                                  ".x/giflib-" version ".tar.bz2"))
              (sha256
               (base32 "0rmp7ipzk42r841bggd7bfqk4p8qsssbp4wcck4qnz7p4rkxbj0a"))))
    (build-system gnu-build-system)
    (outputs '("bin"                    ; utility programs
               "out"))                  ; library
    (inputs `(("libx11" ,libx11)
              ("libice" ,libice)
              ("libsm" ,libsm)
              ("perl" ,perl)))
    (arguments
     `(#:phases (alist-cons-after
                 'unpack 'disable-html-doc-gen
                 (lambda _
                   (substitute* "doc/Makefile.in"
                     (("^all: allhtml manpages") "")))
                 (alist-cons-after
                  'install 'install-manpages
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((bin (assoc-ref outputs "bin"))
                           (man1dir (string-append bin "/share/man/man1")))
                      (mkdir-p man1dir)
                      (for-each (lambda (file)
                                  (let ((base (basename file)))
                                    (format #t "installing `~a' to `~a'~%"
                                            base man1dir)
                                    (copy-file file
                                               (string-append
                                                man1dir "/" base))))
                                (find-files "doc" "\\.1"))))
                  %standard-phases))))
    (synopsis "Tools and library for working with GIF images")
    (description
     "GIFLIB is a library for reading and writing GIF images.  It is API and
ABI compatible with libungif which was in wide use while the LZW compression
algorithm was patented.  Tools are also included to convert, manipulate,
compose, and analyze GIF images.")
    (home-page "http://giflib.sourceforge.net/")
    (license license:x11)))

(define-public imlib2
  (package
    (name "imlib2")
    (version "1.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/enlightenment/imlib2-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0kjggg4gfn6chi8v1xddd5qwk1fbnl7rvd93qiclv5v11s615k0p"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Will be fixed in the next release:
       ;; <http://git.enlightenment.org/legacy/imlib2.git/commit/?id=5dde234b2d3caf067ea827858c53adc5d4c56c13>.
       #:phases (alist-cons-before
                 'configure 'patch-config
                 (lambda _
                   (substitute* "imlib2-config.in"
                     (("@my_libs@") "")))
                 %standard-phases)))
    (native-inputs
     `(("pkgconfig" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("freetype" ,freetype)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("giflib" ,giflib)
       ("bzip2" ,bzip2)))
    (home-page "http://sourceforge.net/projects/enlightenment/")
    (synopsis
     "Loading, saving, rendering and manipulating image files")
    (description
     "Imlib2 is a library that does image file loading and saving as well as
rendering, manipulation, arbitrary polygon support, etc.

It does ALL of these operations FAST.  Imlib2 also tries to be highly
intelligent about doing them, so writing naive programs can be done easily,
without sacrificing speed.

This is a complete rewrite over the Imlib 1.x series.  The architecture is
more modular, simple, and flexible.")
    (license license:imlib2)))

(define-public giblib
  (package
    (name "giblib")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://linuxbrit.co.uk/downloads/giblib-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1b4bmbmj52glq0s898lppkpzxlprq9aav49r06j2wx4dv3212rhp"))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("imlib2" ,imlib2)))
    (home-page "http://linuxbrit.co.uk/software/") ; no real home-page
    (synopsis "Wrapper library for imlib2")
    (description
     "Giblib is a simple library which wraps imlib2's context API, avoiding
all the context_get/set calls, adds fontstyles to the truetype renderer and
supplies a generic doubly-linked list and some string functions.")
    ;; This license removes a clause about X Consortium from the original
    ;; X11 license.
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public freeimage
  (package
   (name "freeimage")
   (version "3.16.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://sourceforge/freeimage/Source%20Distribution/"
                  version "/FreeImage"
                  (string-join (string-split version #\.) "")
                  ".zip"))
            (sha256
             (base32
              "0q1gnjnxgphsh4l8i9rfly4bi8xsczsb9ryzbm8hf38lc3fk5bq3"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases (alist-replace
                'unpack
                (lambda* (#:key source #:allow-other-keys)
                  (and (zero? (system* "unzip" source))
                       (chdir "FreeImage")))
                (alist-delete
                 'configure
                 (alist-cons-before
                  'build 'patch-makefile
                  (lambda* (#:key outputs #:allow-other-keys)
                    (substitute* "Makefile.gnu"
                      (("/usr") (assoc-ref outputs "out"))
                      (("-o root -g root") "")))
                  %standard-phases)))
      #:make-flags '("CC=gcc")
      #:tests? #f)) ; no check target
   (native-inputs
    `(("unzip" ,unzip)))
   ;; Fails to build on MIPS due to assembly code in the source.
   (supported-systems (delete "mips64el-linux" %supported-systems))
   (synopsis "Library for handling popular graphics image formats")
   (description
    "FreeImage is a library for developers who would like to support popular
graphics image formats like PNG, BMP, JPEG, TIFF and others.")
   (license license:gpl2+)
   (home-page "http://freeimage.sourceforge.net")))
