;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2014, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2014, 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017,2019,2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mcrypt)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system scons)
  #:use-module (guix deprecation)
  #:use-module (srfi srfi-1))

(define-public iqa
  (package
    (name "iqa")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/iqa/files/"
                       "1.1.2%20Release/iqa_1.1.2_src.tar.gz/download"))
       (sha256
        (base32 "00mgwy031ammab6bwmd1whhvqv3fxy1cs1igabq0n3ag12zhjs77"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (install-file "build/debug/libiqa.a" lib)
               #t))))))
    (synopsis "Image Quality Assessment")
    (description "IQA is a C library for objectively measuring image/video
quality.  It implements many popular algorithms, such as MS-SSIM, MS-SSIM*,
SIMM, MSE, and PSNR.  It is designed to be fast, accurate, and reliable.  All
code is Valgrind-clean and unit tested.")
    (home-page "https://sourceforge.net/projects/iqa/")
    (license license:bsd-4)))

(define-public libpng
  (package
   (name "libpng")
   (version "1.6.37")  ; Remember to also update libpng-apng if possible!
   (source (origin
            (method url-fetch)
            (uri (list (string-append "mirror://sourceforge/libpng/libpng16/"
                                      version "/libpng-" version ".tar.xz")
                       (string-append
                        "ftp://ftp.simplesystems.org/pub/libpng/png/src"
                        "/libpng16/libpng-" version ".tar.xz")
                       (string-append
                        "ftp://ftp.simplesystems.org/pub/libpng/png/src/history"
                        "/libpng16/libpng-" version ".tar.xz")))
            (sha256
             (base32
              "1jl8in381z0128vgxnvn33nln6hzckl7l7j9nqvkaf1m9n1p0pjh"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags '("--disable-static")))

   ;; libpng.la says "-lz", so propagate it.
   (propagated-inputs `(("zlib" ,zlib)))

   (synopsis "Library for handling PNG files")
   (description
    "Libpng is the official PNG (Portable Network Graphics) reference
library.  It supports almost all PNG features and is extensible.")
   (license license:zlib)
   (home-page "http://www.libpng.org/pub/png/libpng.html")))

;; libpng-apng should be updated when the APNG patch is released:
;; <https://bugs.gnu.org/27556>
(define-public libpng-apng
  (package
    (name "libpng-apng")
    (version "1.6.37")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://sourceforge/libpng/libpng16/"
                                 version "/libpng-" version ".tar.xz")
                  (string-append
                   "ftp://ftp.simplesystems.org/pub/libpng/png/src"
                   "/libpng16/libpng-" version ".tar.xz")
                  (string-append
                   "ftp://ftp.simplesystems.org/pub/libpng/png/src/history"
                   "/libpng16/libpng-" version ".tar.xz")))
       (sha256
        (base32
         "1jl8in381z0128vgxnvn33nln6hzckl7l7j9nqvkaf1m9n1p0pjh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-apng
           (lambda* (#:key inputs #:allow-other-keys)
             (define (apply-patch file)
               (invoke "patch" "-p1" "--force"
                       "--input" file))
             (let ((apng.gz (assoc-ref inputs "apng")))
               (format #t "Applying APNG patch '~a'...~%"
                       apng.gz)
               (invoke "sh" "-c"
                       (string-append "gunzip < " apng.gz " > the-patch"))
               (apply-patch "the-patch")
               #t)))
         (add-before 'configure 'no-checks
           (lambda _
             (substitute* "Makefile.in"
               (("^scripts/symbols.chk") "")
               (("check: scripts/symbols.chk") ""))
             #t)))))
    (inputs
     `(("apng" ,(origin
                  (method url-fetch)
                  (uri
                   (string-append "mirror://sourceforge/libpng-apng/libpng16/"
                                  version "/libpng-" version "-apng.patch.gz"))
                  (sha256
                   (base32
                    "1dh0250mw9b2hx7cdmnb2blk7ddl49n6vx8zz7jdmiwxy38v4fw2"))))))
    (native-inputs
     `(("libtool" ,libtool)))
    ;; libpng.la says "-lz", so propagate it.
    (propagated-inputs
     `(("zlib" ,zlib)))
    (synopsis "APNG patch for libpng")
    (description
     "APNG (Animated Portable Network Graphics) is an unofficial
extension of the APNG (Portable Network Graphics) format.
APNG patch provides APNG support to libpng.")
    (home-page "https://sourceforge.net/projects/libpng-apng/")
    (license license:zlib)))

(define-public libpng-1.2
  (package
    (inherit libpng)
    (version "1.2.59")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://sourceforge/libpng/libpng12/"
                                 version "/libpng-" version ".tar.xz")
                  (string-append
                   "ftp://ftp.simplesystems.org/pub/libpng/png/src"
                   "/libpng12/libpng-" version ".tar.xz")
                  (string-append
                   "ftp://ftp.simplesystems.org/pub/libpng/png/src/history"
                   "/libpng12/libpng-" version ".tar.xz")))
       (sha256
        (base32
         "1izw9ybm27llk8531w6h4jp4rk2rxy2s9vil16nwik5dp0amyqxl"))))))

(define-public pngcrush
  (package
   (name "pngcrush")
   (version "1.8.13")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/pmt/pngcrush/"
                                version "/pngcrush-" version "-nolib.tar.xz"))
            (sha256 (base32
                     "0l43c59d6v9l0g07z3q3ywhb8xb3vz74llv3mna0izk9bj6aqkiv"))))
   (build-system gnu-build-system)
   (arguments
    '(#:tests? #f ; no check target
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "Makefile"
              (("^(PNG(INC|LIB) = )/usr/local/" line vardef)
               (string-append vardef (assoc-ref inputs "libpng") "/"))
              (("^(Z(INC|LIB) = )/usr/local/" line vardef)
               (string-append vardef (assoc-ref inputs "zlib") "/"))
              ;; The Makefile is written by hand and not using $PREFIX
              (("\\$\\(DESTDIR\\)/usr/")
               (string-append (assoc-ref outputs "out") "/")))
            #t)))))
   (inputs
    `(("libpng" ,libpng)
      ("zlib" , zlib)))
   (home-page "https://pmt.sourceforge.io/pngcrush")
   (synopsis "Utility to compress PNG files")
   (description "Pngcrush optimizes @acronym{PNG, Portable Network Graphics}
images.  It can further losslessly compress them by as much as 40%.")
   (license license:zlib)))

(define-public pnglite
  (let ((commit "11695c56f7d7db806920bd9229b69f230e6ffb38")
        (revision "1"))
    (package
      (name "pnglite")
      ;; The project was moved from sourceforge to github.
      ;; The latest version in sourceforge was 0.1.17:
      ;; https://sourceforge.net/projects/pnglite/files/pnglite/
      ;; No releases are made in github.
      (version (git-version "0.1.17" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dankar/pnglite")
                      (commit commit)))
                (sha256
                 (base32
                  "1lmmkdxby5b8z9kx3zrpgpk33njpcf2xx8z9bgqag855sjsqbbby"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda _
               ;; common build flags for building shared libraries
               (let ((cflags '("-O2" "-g" "-fPIC"))
                     (ldflags '("-shared")))
                 (apply invoke
                        `("gcc"
                          "-o" "libpnglite.so"
                          ,@cflags
                          ,@ldflags
                          "pnglite.c"))
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/"))
                      (include (string-append out "/include/"))
                      (doc (string-append out "/share/doc/"
                                          ,name "-" ,version "/")))
                 (install-file "libpnglite.so" lib)
                 (install-file "pnglite.h" include)
                 (install-file "README.md" doc)
                 #t))))))
      (inputs `(("zlib" ,zlib)))
      (home-page "https://github.com/dankar/pnglite")
      (synopsis "Pretty small png library")
      (description "A pretty small png library.
Currently all documentation resides in @file{pnglite.h}.")
      (license license:zlib))))

(define-public libimagequant
  (package
    (name "libimagequant")
    (version "2.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ImageOptim/libimagequant")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00w7fny3xf14cfyhbdnmqyh9ddqdh1irvgzxd35a2z65kp7vnvj0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f))                    ; no check target
    (home-page "https://pngquant.org/lib/")
    (synopsis "Image palette quantization library")
    (description "libimagequant is a small, portable C library for
high-quality conversion of RGBA images to 8-bit indexed-color (palette)
images.  This library can significantly reduces file sizes and powers pngquant
and other PNG optimizers.")
    (license license:gpl3+)))

(define-public pngquant
  (package
    (name "pngquant")
    (version "2.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kornelski/pngquant")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15hanshahxqs6s9fyc3aym02251dcys7bf78g3inp0y233amdbl3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags
       '("--with-openmp" "--with-lcms2")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libpng" ,libpng)
       ("zlib" , zlib)
       ("lcms" ,lcms)
       ("libimagequant" ,libimagequant)))
    (home-page "https://pngquant.org/")
    (synopsis "Utility and library for lossy compressing PNG images")
    (description "pngquant is a PNG compressor that significantly reduces file
sizes by converting images to a more efficient 8-bit PNG format with alpha
channel (often 60-80% smaller than 24/32-bit PNG files).  Compressed images
are fully standards-compliant and are supported by all web browsers and
operating systems.

Features:
@enumerate
@item High-quality palette generation using a combination of vector
      quantization algorithms.
@item Unique adaptive dithering algorithm that adds less noise to images
      than the standard Floyd-Steinberg.
@item Easy to integrate with shell scripts, GUIs and server-side software.
@item Fast mode for real-time processing/large numbers of images.
@end enumerate")
    (license license:gpl3+)))

(define-public ijg-libjpeg
  (package
   (name "libjpeg")
   (version "9d")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.ijg.org/files/jpegsrc.v"
                   version ".tar.gz"))
            (sha256 (base32
                     "0clwys9lcqlxqgcw8s1gwfm5ix2zjlqpklmd3mbvqmj5ibj51jwr"))))
   (build-system gnu-build-system)
   (synopsis "Library for handling JPEG files")
   (description
    "Libjpeg implements JPEG image encoding, decoding, and transcoding.
JPEG is a standardized compression method for full-color and gray-scale
images.
It also includes programs that provide conversion between the JPEG format and
image files in PBMPLUS PPM/PGM, GIF, BMP, and Targa file formats, as well as
lossless JPEG manipulations such as rotation, scaling or cropping:
@enumerate
@item cjpeg
@item djpeg
@item jpegtran
@item rdjpgcom
@item wrjpgcom
@end enumerate")
   (license license:ijg)
   (home-page "https://www.ijg.org/")))

(define-public libjxr
  (package
    (name "libjxr")
    (version "1.1")
    (source (origin
              ;; We are using the Debian source because CodePlex does not
              ;; deliver an easily downloadable tarball.
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/j/jxrlib/jxrlib_"
                                  version ".orig.tar.gz"))
              (sha256
               (base32
                "00w3f3cmjsm3fiaxq5mxskmp5rl3mki8psrf9y8s1vqbg237na67"))
              (patch-flags '("-p1" "--binary"))
              (patches (search-patches "libjxr-fix-function-signature.patch"
                                       "libjxr-fix-typos.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list "CC=gcc"
             ;; A substitute* procedure call would be enough to add the -fPIC
             ;; flag if there was no file decoding error.
             ;; The makefile is a "Non-ISO extended-ASCII text, with CRLF line
             ;; terminators" according to the file(1) utility.
             (string-append "CFLAGS=-I. -Icommon/include -Iimage/sys -fPIC "
                            "-D__ANSI__ -DDISABLE_PERF_MEASUREMENT -w -O "))
       #:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (add-after 'build 'build-shared-library
           (lambda _
             ;; The Makefile uses optimization level 1, so the same
             ;; level is used here for consistency.
             (invoke "gcc" "-shared" "-fPIC" "-O"
                     ;; Common files.
                     "adapthuff.o" "image.o" "strcodec.o" "strPredQuant.o"
                     "strTransform.o" "perfTimerANSI.o"
                     ;; Decoding files.
                     "decode.o" "postprocess.o" "segdec.o" "strdec.o"
                     "strInvTransform.o" "strPredQuantDec.o" "JXRTranscode.o"
                     ;; Encoding files.
                     "encode.o" "segenc.o" "strenc.o" "strFwdTransform.o"
                     "strPredQuantEnc.o"
                     "-o" "libjpegxr.so")
             (invoke "gcc" "-shared" "-fPIC" "-O"
                     ;; Glue files.
                     "JXRGlue.o" "JXRMeta.o" "JXRGluePFC.o" "JXRGlueJxr.o"
                     ;; Test files.
                     "JXRTest.o" "JXRTestBmp.o" "JXRTestHdr.o" "JXRTestPnm.o"
                     "JXRTestTif.o" "JXRTestYUV.o"
                     "-o" "libjxrglue.so")))
         ;; The upstream makefile does not include an install phase.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include/jxrlib")))
               (for-each (lambda (file)
                           (install-file file include)
                           (delete-file file))
                         (append
                          '("jxrgluelib/JXRGlue.h"
                            "jxrgluelib/JXRMeta.h"
                            "jxrtestlib/JXRTest.h"
                            "image/sys/windowsmediaphoto.h")
                          (find-files "common/include" "\\.h$")))
               (for-each (lambda (file)
                           (install-file file lib)
                           (delete-file file))
                         (find-files "." "\\.(a|so)$"))
               (for-each (lambda (file)
                           (install-file file bin)
                           (delete-file file))
                         '("JxrDecApp" "JxrEncApp")))
             #t)))))
    (synopsis "Implementation of the JPEG XR standard")
    (description "JPEG XR is an approved ISO/IEC International standard (its
official designation is ISO/IEC 29199-2). This library is an implementation of that standard.")
    (license
     (license:non-copyleft
      "file://Makefile"
      "See the header of the Makefile in the distribution."))
    (home-page "https://jxrlib.codeplex.com/")))

(define-public jpegoptim
  (package
   (name "jpegoptim")
   (version "1.4.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.kokkonen.net/tjko/src/jpegoptim-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1dss7907fclfl8zsw0bl4qcw0hhz6fqgi3867w0jyfm3q9jfpcc8"))))
   (build-system gnu-build-system)
   (inputs `(("libjpeg" ,libjpeg-turbo)))
   (arguments
    '(#:tests? #f))                     ; no tests
   (synopsis "Optimize JPEG images")
   (description
    "jpegoptim provides lossless optimization (based on optimizing
the Huffman tables) and \"lossy\" optimization based on setting
maximum quality factor.")
   (license license:gpl2+)
   (home-page "https://www.kokkonen.net/tjko/projects.html#jpegoptim")))

(define-public libicns
  (package
    (name "libicns")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/icns/"
                    "libicns-" version ".tar.gz"))
              (sha256
               (base32
                "1hjm8lwap7bjyyxsyi94fh5817xzqhk4kb5y0b7mb6675xw10prk"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("jasper" ,jasper)))
    (arguments
     `(#:tests? #t)) ; No tests.
    (home-page "http://icns.sourceforge.net/")
    (synopsis "Library for handling Mac OS icns resource files")
    (description
     "Libicns is a library for the manipulation of Mac OS IconFamily resource
type files (ICNS).  @command{icns2png} and @command{png2icns} are provided to
convert between PNG and ICNS. @command{icns2png} will extract image files from
ICNS files under names like \"Foo_48x48x32.png\" useful for installing for use
with .desktop files.  Additionally, @command{icontainer2png} is provided for
extracting icontainer icon files.")
    (license (list license:lgpl2.1+     ; libicns
                   license:lgpl2.0+     ; src/apidocs.*
                   license:gpl2+))))    ; icns2png, png2icns, icontainer2png

(define-public libtiff
  (package
   (name "libtiff")
   (version "4.1.0")
   (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.osgeo.org/libtiff/tiff-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0d46bdvxdiv59lxnb0xz9ywm8arsr6xsapi5s6y6vnys2wjz6aax"))))
   (replacement libtiff/fixed)
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"))                           ;1.3 MiB of HTML documentation
   (arguments
    ;; Instead of using --docdir, this package has its own --with-docdir.
    `(#:configure-flags
      (list (string-append "--with-docdir="
                           (assoc-ref %outputs "doc")
                           "/share/doc/"
                           ,name "-" ,(package-version this-package))
            "--disable-static")))
   (inputs `(("zlib" ,zlib)
             ("libjpeg" ,libjpeg-turbo)))
   (synopsis "Library for handling TIFF files")
   (description
    "Libtiff provides support for the Tag Image File Format (TIFF), a format
used for storing image data.
Included are a library, libtiff, for reading and writing TIFF and a small
collection of tools for doing simple manipulations of TIFF images.")
   (license (license:non-copyleft "file://COPYRIGHT"
                                  "See COPYRIGHT in the distribution."))
   (home-page "http://www.simplesystems.org/libtiff/")))

(define-public libtiff/fixed
  (package
    (inherit libtiff)
    (name "libtiff")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.osgeo.org/libtiff/tiff-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jrkjv0xya9radddn8idxvs2gqzp3l2b1s8knlizmn7ad3jq817b"))))))

(define-public leptonica
  (package
    (name "leptonica")
    (version "1.80.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DanBloomberg/leptonica")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12ddln72z5l3icz0i9rpsfkg5xik8fcwcn8lb0cp3jigjxi8gvkg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gnuplot" ,gnuplot)             ;needed for test suite
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("openjpeg" ,openjpeg)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-reg-wrapper
           (lambda _
             (substitute* "prog/reg_wrapper.sh"
               ((" /bin/sh ")
                (string-append " " (which "sh") " "))
               (("which gnuplot")
                "true"))
             #t))
         (add-after 'install 'provide-absolute-giflib-reference
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (giflib (assoc-ref inputs "giflib")))
               ;; Add an absolute reference to giflib to avoid propagation.
               (with-directory-excursion (string-append out "/lib")
                 (substitute* '("liblept.la" "pkgconfig/lept.pc")
                   (("-lgif") (string-append "-L" giflib "/lib -lgif"))))
               #t))))))
    (home-page "http://www.leptonica.com/")
    (synopsis "Library and tools for image processing and analysis")
    (description
     "Leptonica is a C library and set of command-line tools for efficient
image processing and image analysis operations.  It supports rasterop, affine
transformations, binary and grayscale morphology, rank order, and convolution,
seedfill and connected components, image transformations combining changes in
scale and pixel depth, and pixelwise masking, blending, enhancement, and
arithmetic ops.")
    (license license:bsd-2)))

(define-public jbig2dec
  (package
    (name "jbig2dec")
    (version "0.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ArtifexSoftware"
                                  "/ghostpdl-downloads/releases/download"
                                  "/gs951/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pigfw2v0ppvr0lbysm69gx0zsa5q2q92yrb8af2j3im6x97f6cy"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--disable-static")
                 #:phases (modify-phases %standard-phases
                            (add-before 'bootstrap 'force-bootstrap
                              (lambda _
                                ;; XXX: jbig2dec 0.18 was released with
                                ;; a broken configure script, so we
                                ;; recreate the build system here.
                                ;; Remove the autoconf inputs below
                                ;; when deleting this code.
                                (delete-file "configure")
                                (delete-file "autogen.sh")
                                #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("python" ,python-wrapper)))     ;for tests
    (synopsis "Decoder of the JBIG2 image compression format")
    (description
      "JBIG2 is designed for lossy or lossless encoding of @code{bilevel} (1-bit
monochrome) images at moderately high resolution, and in particular scanned
paper documents.  In this domain it is very efficient, offering compression
ratios on the order of 100:1.

This is a decoder only implementation, and currently is in the alpha
stage, meaning it doesn't completely work yet.  However, it is
maintaining parity with available encoders, so it is useful for real
work.")
    (home-page "https://jbig2dec.com")
    (license license:gpl2+)))

(define-public jbigkit
  (package
    (name "jbigkit")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.cl.cam.ac.uk/~mgk25/jbigkit/"
                           "download/jbigkit-" version ".tar.gz"))
       (sha256
        (base32 "0cnrcdr1dwp7h7m0a56qw09bv08krb37mpf7cml5sjdgpyv0cwfy"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove files without clear licence information.
           (for-each delete-file-recursively
                     (list "contrib" "examples"))
           #t))))
    (build-system gnu-build-system)
    (outputs (list "out" "pbmtools"))
    (arguments
     `(#:modules ((srfi srfi-26)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install              ; no ‘make install’ target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (with-directory-excursion "libjbig"
                 (for-each (cut install-file <> include)
                           (find-files "." "\\.h$"))
                 (for-each (cut install-file <> lib)
                           (find-files "." "\\.a$")))
               #t)))
         (add-after 'install 'install-pbmtools
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "pbmtools"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1"))
                    (man5 (string-append out "/share/man/man5")))
               (with-directory-excursion "pbmtools"
                 (for-each (cut install-file <> bin)
                           (list "jbgtopbm" "jbgtopbm85"
                                 "pbmtojbg" "pbmtojbg85"))

                 (for-each (cut install-file <> man1)
                           (find-files "." "\\.1$"))
                 (for-each (cut install-file <> man5)
                           (find-files "." "\\.5$"))
                 #t)))))
       #:test-target "test"
       #:tests? #f))                    ; tests depend on examples/
    (home-page "https://www.cl.cam.ac.uk/~mgk25/jbigkit/")
    (synopsis "Lossless compression for bi-level high-resolution images")
    (description
     "JBIG-KIT implements the JBIG1 data compression standard (ITU-T T.82 and
ISO/IEC 11544:1993), designed for bi-level (one bit per pixel) images such as
black-and-white scanned documents.  It is widely used in fax products, printer
firmware and drivers, document management systems, and imaging software.

This package provides a static C library of (de)compression functions and some
simple command-line converters similar to those provided by netpbm.

Two JBIG1 variants are available.  One (@file{jbig.c}) implements nearly all
options of the standard but has to keep the full uncompressed image in memory.
The other (@file{jbig85.c}) implements just the ITU-T T.85 profile, with
memory management optimized for embedded and fax applications.  It buffers
only a few lines of the uncompressed image in memory and is able to stream
images of initially unknown height.")
    (license (list license:isc          ; pbmtools/p?m.5
                   license:gpl2+))))    ; the rest

(define-public openjpeg-data
  (package
    (name "openjpeg-data")
    (version "2020.05.19")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/uclouvain/openjpeg-data")
         (commit "c5c4a8c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jp84gbhw8q5b8mhc322ql9410hjf32w9hg10x4isfa9j59mnncb"))))
    (build-system copy-build-system)
    (synopsis "Test files for OpenJPEG")
    (description "OpenJPEG-Data contains all files required to run the openjpeg
test suite, including conformance tests (following Rec. ITU-T T.803 | ISO/IEC
15444-4 procedures), non-regression tests and unit tests.")
    (home-page "https://github.com/uclouvain/openjpeg-data")
    (license license:bsd-2)))

(define-public openjpeg
  (package
    (name "openjpeg")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/uclouvain/openjpeg")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name "openjpeg" version))
       (sha256
        (base32 "1dn98d2dfa1lqyxxmab6rrcv52dyhjr4g7i4xf2w54fqsx14ynrb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f           ;TODO: requires a 1.1 GiB data repository
       #:configure-flags '("-DBUILD_STATIC_LIBS=OFF")))
    (inputs
     `(("lcms" ,lcms)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("zlib" ,zlib)))
    (synopsis "OPENJPEG Library and Applications")
    (description "OpenJPEG is an implementation of JPEG 2000 codec written in C
language.  It has been developed in order to promote the use of JPEG 2000, a
still-image compression standard from the Joint Photographic Experts Group
(JPEG).  Since April 2015, it is officially recognized by ISO/IEC and ITU-T as a
JPEG 2000 Reference Software.")
    (home-page "https://github.com/uclouvain/openjpeg")
    (license license:bsd-2)))

(define-public openjpeg-1
  (package (inherit openjpeg)
    (name "openjpeg")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/openjpeg.mirror/" version "/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "11waq9w215zvzxrpv40afyd18qf79mxc28fda80bm3ax98cpppqm"))))))

(define-public giflib
  (package
    (name "giflib")
    (version "5.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/giflib/giflib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1gbrg03z1b6rlrvjyc6d41bc8j1bsr7rm8206gb1apscyii5bnii"))))
    (build-system gnu-build-system)
    (outputs '("bin"                    ; utility programs
               "out"))                  ; library
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          (string-append "BINDIR="
                                         (assoc-ref %outputs "bin") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-html-doc-gen
           (lambda _
             (substitute* "doc/Makefile"
               (("^all: allhtml manpages") ""))
             #t))
         (delete 'configure)
         (add-after 'install 'install-manpages
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
                         (find-files "doc" "\\.1"))
               #t))))))
    (synopsis "Tools and library for working with GIF images")
    (description
     "GIFLIB is a library for reading and writing GIF images.  It is API and
ABI compatible with libungif which was in wide use while the LZW compression
algorithm was patented.  Tools are also included to convert, manipulate,
compose, and analyze GIF images.")
    (home-page "http://giflib.sourceforge.net/")
    (license license:x11)))

(define-public libuemf
  (package
    (name "libuemf")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libuemf/libUEMF-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05djs99vqf067x81xfpskh7a66y5x7b4mmjavybcy7swnm0swg7v"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Overriding CMAKE_INSTALL_PREFIX is not a good idea.
         (add-after 'unpack 'fix-CMakeLists.txt
           (lambda _
             (substitute* "CMakeLists.txt"
               ((".*SET\\(CMAKE_INSTALL_PREFIX.*") ""))
             #t))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sources (string-append "../libUEMF-" ,version))
                    (drm-tools (assoc-ref inputs "drm-tools"))
                    (extract (string-append drm-tools "/bin/extract"))
                    (execinput (string-append drm-tools "/bin/execinput")))
               (with-directory-excursion sources
                 (substitute* "testit.sh"
                   (("^EPATH=.*")
                    (format #f "EPATH=~a~%" bin))
                   (("`which diff`")
                    "diff")
                   (("^EXTRACT=.*")
                    (format #f "EXTRACT=~a~%" extract))
                   (("^EXECINPUT=.*")
                    (format #f "EXECINPUT=~a~%" execinput)))
                 (invoke "sh" "testit.sh"))))))))
    (native-inputs `(("drm-tools" ,drm-tools))) ;for tests
    (home-page "http://libuemf.sourceforge.net/")
    (synopsis "Library for working with WFM, EMF and EMF+ images")
    (description "The libUEMF library is a portable C99 implementation for
reading and writing @acronym{WFM, Windows Metafile}, @acronym{EMF, Enhanced
Metafile}, and @acronym{EMF+, Enhanced Metafile Plus} files.")
    (license license:gpl2+)))

(define-public libungif
  (package
    (name "libungif")
    (version "4.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/giflib/libungif-4.x/"
                                  "libungif-" version "/libungif-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0cnksimmmjngdrys302ik1385sg1sj4i0gxivzldhgwd46n7x2kh"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))          ;package ships some perl tools
    (home-page "http://giflib.sourceforge.net/")
    (synopsis "GIF decompression library")
    (description
     "libungif is the old GIF decompression library by the GIFLIB project.")
    (license license:expat)))

(define-public imlib2
  (package
    (name "imlib2")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/enlightenment/imlib2-src/" version
                    "/imlib2-" version ".tar.bz2"))
              (sha256
               (base32
                "01y45cdml2dr9cqgybrgxr86sd77d1qfa1gzclzy1j6bkminlfh3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list "--disable-static")))
    (native-inputs
     `(("pkgconfig" ,pkg-config)))
    (inputs
     `(("bzip2" ,bzip2)
       ("freetype" ,freetype)
       ("giflib" ,giflib)
       ("libid3tag" ,libid3tag)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libwebp" ,libwebp)))
    (home-page "https://sourceforge.net/projects/enlightenment/")
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
              (uri (list
                     (string-append
                       "http://linuxbrit.co.uk/downloads/giblib-"
                       version ".tar.gz")
                     (string-append
                       "https://src.fedoraproject.org/repo/pkgs/giblib/giblib-"
                       version ".tar.gz/c810ef5389baf24882a1caca2954385e/giblib-"
                       version ".tar.gz")
                     (string-append
                       "https://sourceforge.net/projects/slackbuildsdirectlinks/"
                       "files/giblib/giblib-" version ".tar.gz")))
              (sha256
               (base32
                "1b4bmbmj52glq0s898lppkpzxlprq9aav49r06j2wx4dv3212rhp"))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("imlib2" ,imlib2)))
    (home-page
     ;; This vanished page is universally accepted as giblib's home despite not
     ;; mentioning the package once.
     (string-append "https://web.archive.org/web/20140907071208/"
                    "https://linuxbrit.co.uk/software/"))
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
   (version "3.18.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://sourceforge/freeimage/Source%20Distribution/"
                  version "/FreeImage"
                  (string-concatenate (string-split version #\.))
                  ".zip"))
            (sha256
             (base32
              "1z9qwi9mlq69d5jipr3v2jika2g0kszqdzilggm99nls5xl7j4zl"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (for-each
                  (lambda (dir)
                    (delete-file-recursively (string-append "Source/" dir)))
                  '("LibJPEG" "LibOpenJPEG" "LibPNG" "LibRawLite"
                    "LibJXR" "LibWebP" "OpenEXR" "ZLib"))))
            (patches
             (append
              (search-patches "freeimage-unbundle.patch")
              ;; Take one patch from Arch Linux that adds LibRaw 0.20 compatibility.
              (list (origin
                      (method url-fetch)
                      (uri "https://raw.githubusercontent.com/archlinux\
/svntogit-community/ca3e6a52f5a46dec87cbf85e9d84fe370e282c8c/trunk\
/freeimage-libraw-0.20.patch")
                      (file-name "freeimage-libraw-compat.patch")
                      (sha256
                       (base32
                        "0cwjxjz0f4gs6igvwqg0p99mnrsrwzkal1l2n08yvz2xq9s5khki"))))))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        ;; According to Fedora these files depend on private headers, but their
        ;; presence is required for building, so we replace them with empty files.
        (add-after 'unpack 'delete-unbuildable-files
          (lambda _
            (for-each (lambda (file)
                        (delete-file file)
                        (close (open file O_CREAT)))
                      '("Source/FreeImage/PluginG3.cpp"
                        "Source/FreeImageToolkit/JPEGTransform.cpp"))
            #t))
        ;; These scripts generate the Makefiles.
        (replace 'configure
          (lambda _
            (invoke "sh" "gensrclist.sh")
            (invoke "sh" "genfipsrclist.sh")))
        (add-before 'build 'patch-makefile
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "Makefile.gnu"
              (("/usr") (assoc-ref outputs "out"))
              (("-o root -g root") ""))
            #t)))
      #:make-flags
      (list "CC=gcc"
            ;; We need '-fpermissive' for Source/FreeImage.h.
            ;; libjxr doesn't have a pkg-config file.
            (string-append "CFLAGS+=-O2 -fPIC -fvisibility=hidden -fpermissive "
                           "-I" (assoc-ref %build-inputs "libjxr") "/include/jxrlib"))
      #:tests? #f)) ; no check target
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("unzip" ,unzip)))
   (inputs
    `(("libjpeg" ,libjpeg-turbo)
      ("libjxr" ,libjxr)
      ("libpng" ,libpng)
      ("libraw" ,libraw)
      ("libtiff" ,libtiff)
      ("libwebp" ,libwebp)
      ("openexr" ,openexr)
      ("openjpeg" ,openjpeg)
      ("zlib" ,zlib)))
   (synopsis "Library for handling popular graphics image formats")
   (description
    "FreeImage is a library for developers who would like to support popular
graphics image formats like PNG, BMP, JPEG, TIFF and others.")
   (license license:gpl2+)
   (home-page "http://freeimage.sourceforge.net")))

(define-public vigra
  (package
   (name "vigra")
   (version "1.11.1")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://github.com/ukoethe/vigra/releases/download/"
                          "Version-" (string-join (string-split version #\.) "-")
                          "/vigra-" version "-src.tar.gz"))
      (patches (search-patches "vigra-python-compat.patch"))
      (sha256 (base32
                "1bqs8vx5i1bzamvv563i24gx2xxdidqyxh9iaj46mbznhc84wmm5"))))
   (build-system cmake-build-system)
   (inputs
    `(("boost" ,boost)
      ("fftw" ,fftw)
      ("fftwf" ,fftwf)
      ("hdf5" ,hdf5)
      ("ilmbase" ,ilmbase) ; propagated by openexr, but needed explicitly
                           ; to create a configure-flag
      ("libjpeg" ,libjpeg-turbo)
      ("libpng" ,libpng)
      ("libtiff" ,libtiff)
      ("openexr" ,openexr)
      ("python" ,python-wrapper)
      ("python-numpy" ,python-numpy)
      ("zlib" ,zlib)))
   (native-inputs
    `(("doxygen" ,doxygen)
      ("python-nose" ,python-nose)
      ("sphinx" ,python-sphinx)))
   (arguments
    `(#:test-target "check"
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'disable-broken-tests
          (lambda _
            ;; See https://github.com/ukoethe/vigra/issues/432
            (substitute* "test/fourier/CMakeLists.txt"
              (("VIGRA_ADD_TEST.*") ""))
            ;; This test fails with Numpy 1.15:
            ;; <https://github.com/ukoethe/vigra/issues/436>.
            (substitute* "vigranumpy/test/CMakeLists.txt"
              (("test1\\.py") ""))
            #t)))
      #:configure-flags
        (list "-Wno-dev" ; suppress developer mode with lots of warnings
              (string-append "-DVIGRANUMPY_INSTALL_DIR="
                             (assoc-ref %outputs "out")
                             "/lib/python"
                             ,(version-major+minor (package-version python))
                             "/site-packages")
              ;; OpenEXR is not enabled by default.
              "-DWITH_OPENEXR=1"
              ;; Fix rounding error on 32-bit machines
              "-DCMAKE_C_FLAGS=-ffloat-store"
              ;; The header files of ilmbase are not found when included
              ;; by the header files of openexr, and an explicit flag
              ;; needs to be set.
              (string-append "-DCMAKE_CXX_FLAGS=-I"
                             (assoc-ref %build-inputs "ilmbase")
                             "/include/OpenEXR"
                             " -ffloat-store"))))
   (synopsis "Computer vision library")
   (description
    "VIGRA stands for Vision with Generic Algorithms.  It is an image
processing and analysis library that puts its main emphasis on customizable
algorithms and data structures.  It is particularly strong for
multi-dimensional image processing.")
   (license license:expat)
   (home-page "https://ukoethe.github.io/vigra/")
   (properties '((max-silent-time . 7200))))) ;2 hours, to avoid timing out


(define-public vigra-c
  (let* ((commit "66ff4fa5a7d4a77415caa676a45c2c6ea16562e7")
         (revision "1"))
    (package
      (name "vigra-c")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/BSeppke/vigra_c")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1pnd92s284dvsg8zp6md7p8ck55bmcsryz58gzic7jh6m72hg689"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f))                  ; No test target.
      (native-inputs
       `(("doxygen" ,doxygen)))
      (inputs
       `(("fftw" ,fftw)
         ("fftwf" ,fftwf)
         ("hdf5" ,hdf5)
         ("vigra" ,vigra)))
      (synopsis "C interface to the VIGRA computer vision library")
      (description
       "This package provides a C interface to the VIGRA C++ computer vision
library.  It is designed primarily to ease the implementation of higher-level
language bindings to VIGRA.")
      (license license:expat))))

(define-public libwebp
  (package
    (name "libwebp")
    (version "1.1.0")
    (source
     (origin
       ;; No tarballs are provided for >0.6.1.
       (method git-fetch)
       (uri (git-reference
             (url "https://chromium.googlesource.com/webm/libwebp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r2yy9if0ndvpzadk39bigvsygyqnlv0xjb9w2aj6rs534mncazz"))))
    (build-system gnu-build-system)
    (inputs
     `(("freeglut" ,freeglut)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     '(#:configure-flags '("--enable-libwebpmux"
                           "--enable-libwebpdemux"
                           "--enable-libwebpdecoder"
                           "--disable-static")))
    (home-page "https://developers.google.com/speed/webp/")
    (synopsis "Lossless and lossy image compression")
    (description
     "WebP is a new image format that provides lossless and lossy compression
for images.  WebP lossless images are 26% smaller in size compared to
PNGs.  WebP lossy images are 25-34% smaller in size compared to JPEG images at
equivalent SSIM index.  WebP supports lossless transparency (also known as
alpha channel) with just 22% additional bytes.  Transparency is also supported
with lossy compression and typically provides 3x smaller file sizes compared
to PNG when lossy compression is acceptable for the red/green/blue color
channels.")
    (license license:bsd-3)))

(define-public libmng
  (package
    (name "libmng")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libmng/libmng-devel/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lvxnpds0vcf0lil6ia2036ghqlbl740c4d2sz0q5g6l93fjyija"))))
    (build-system gnu-build-system)
    (propagated-inputs
     ;; These are all in the 'Libs.private' field of libmng.pc.
     `(("lcms" ,lcms)
       ("libjpeg" ,libjpeg-turbo)
       ("zlib" ,zlib)))
    (home-page "https://www.libmng.com/")
    (synopsis "Library for handling MNG files")
    (description
     "Libmng is the MNG (Multiple-image Network Graphics) reference library.")
    (license license:bsd-3)))

(define-public exiv2
  (package
    (name "exiv2")
    (version "0.27.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.exiv2.org/builds/exiv2-" version
                           "-Source.tar.gz"))
       (sha256
        (base32 "0y77wfadjsrcxijdqgkr3q88b6mm9y3rg8kqsmaig8iah49md7x7"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ; no test suite
    (propagated-inputs
     `(("expat" ,expat)
       ("zlib" ,zlib)))
    (home-page "https://www.exiv2.org/")
    (synopsis "Library and command-line utility to manage image metadata")
    (description
     "Exiv2 is a C++ library and a command line utility to manage image
metadata.  It provides fast and easy read and write access to the Exif, IPTC
and XMP metadata of images in various formats.")

    ;; Files under `xmpsdk' are a copy of Adobe's XMP SDK, licensed under the
    ;; 3-clause BSD license: <http://www.adobe.com/devnet/xmp/sdk/eula.html>.
    ;; The core is GPLv2+:
    ;;   <https://launchpad.net/ubuntu/precise/+source/exiv2/+copyright>.
    (license license:gpl2+)))

(define-public devil
  (package
    (name "devil")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/openil/"
                                  "DevIL-" version ".tar.gz"))
              (sha256
               (base32
                "02dpzvi493r09c9hfjnk54nladl3qw55iqkkg18g12fxwwz9fx80"))))
    (build-system cmake-build-system)
    (arguments
     '(;; XXX: Not supported in the released CMakeLists.txt.
       ;; Enable this for > 1.8.0.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'change-directory
           (lambda _ (chdir "DevIL") #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("lcms" ,lcms)
       ("libjpeg" ,libjpeg-turbo)
       ("libmng" ,libmng)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("openexr" ,openexr)
       ("zlib" ,zlib)))
    (synopsis "Library for manipulating many image formats")
    (description "Developer's Image Library (DevIL) is a library to develop
applications with support for many types of images.  DevIL can load, save,
convert, manipulate, filter and display a wide variety of image formats.")
    (home-page "http://openil.sourceforge.net")
    (license license:lgpl2.1+)))

(define-public jasper
  (package
    (name "jasper")
    (version "2.0.27")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdadams/jasper")
                    (commit (string-append "version-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mrnazk8qla7nn59xad86gmrf5fzqcv74j5xhcdrxbgfw67l17zd"))))
    (build-system cmake-build-system)
    (inputs
     `(("libjpeg" ,libjpeg-turbo)))
    (synopsis "JPEG-2000 library")
    (description "The JasPer Project is an initiative to provide a reference
implementation of the codec specified in the JPEG-2000 Part-1 standard (i.e.,
ISO/IEC 15444-1).")
    (home-page "https://www.ece.uvic.ca/~frodo/jasper/")
    (license (license:x11-style "file://LICENSE"))))

(define-public zimg
  (package
    (name "zimg")
    (version "2.9.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/sekrit-twc/zimg")
              (commit (string-append "release-" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1dqyrq3p8bkgvj4ci50ac342hjnhyz6xxvhiwp7wpi3v3nbj7s02"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (synopsis "Scaling, colorspace conversion, and dithering library")
    (description "Zimg implements the commonly required image processing basics
of scaling, colorspace conversion, and depth conversion.  A simple API enables
conversion between any supported formats to operate with minimal knowledge from
the programmer.")
    (home-page "https://github.com/sekrit-twc/zimg")
    ;; test/extra/ contains musl-libm,
    ;; which is MIT/expat licensed, but only used for tests
    (license license:wtfpl2)))

(define-public perceptualdiff
  (package
    (name "perceptualdiff")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/myint/perceptualdiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yys55f9i9g3wjjg0j2m0p0k21zwnid8520a8lrr30khm4k5gibp"))))
    (build-system cmake-build-system)
    (inputs `(("freeimage" ,freeimage)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-tests
                    ;; cmake-build-system uses a build/ directory outside
                    ;; of the source tree, one level higher than expected
                    (lambda _
                      (substitute* "test/run_tests.bash"
                        (("../build") "../../build"))
                      #t)))))
    (home-page "https://github.com/myint/perceptualdiff")
    (synopsis "Perceptual image comparison utility")
    (description "PerceptualDiff visually compares two images to determine
whether they look alike.  It uses a computational model of the human visual
system to detect similarities.  This allows it too see beyond irrelevant
differences in file encoding, image quality, and other small variations.")
    (license license:gpl2+)))

(define-public steghide
  (package
    (name "steghide")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/steghide/steghide/"
                                  version "/steghide-" version ".tar.bz2"))
              (sha256
               (base32
                "18bxlhbdc3zsmxj84i417xjh0q28kv26q449k23n0a72ldwziix2"))
              (patches (list (search-patch "steghide-fixes.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("perl" ,perl)))                 ;for tests
    (inputs
     `(("libmhash" ,libmhash)
       ("libmcrypt" ,libmcrypt)
       ("libjpeg" ,libjpeg-turbo)
       ("zlib" ,zlib)))
    (arguments
     `(#:make-flags '("CXXFLAGS=-fpermissive")    ;required for MHashPP.cc

       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-perl-search-path
                    (lambda _
                      ;; Work around "dotless @INC" build failure.
                      (setenv "PERL5LIB"
                              (string-append (getcwd) "/tests:"
                                             (getenv "PERL5LIB")))
                      #t)))))
    (home-page "http://steghide.sourceforge.net")
    (synopsis "Image and audio steganography")
    (description
     "Steghide is a program to hide data in various kinds of image and audio
files (known as @dfn{steganography}).  Neither color nor sample frequencies are
changed, making the embedding resistant against first-order statistical tests.")
    (license license:gpl2+)))

(define-public optipng
  (package
    (name "optipng")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://prdownloads.sourceforge.net/optipng/optipng-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0lj4clb851fzpaq446wgj0sfy922zs5l5misbpwv6w7qrqrz4cjg"))
       (modules '((guix build utils)))
       (snippet
         '(begin
            (delete-file-recursively "src/libpng")
            (delete-file-recursively "src/zlib")
            #t))))
    (build-system gnu-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; configure script doesn't accept arguments CONFIG_SHELL and SHELL
             (invoke "sh" "configure"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "-with-system-libs")
             #t)))))
    (synopsis "Optimizer that recompresses PNG image files to a smaller size")
    (description "OptiPNG is a PNG optimizer that recompresses image
files to a smaller size, without losing any information.  This program
also converts external formats (BMP, GIF, PNM and TIFF) to optimized
PNG, and performs PNG integrity checks and corrections.")
    (home-page "http://optipng.sourceforge.net/")
    (license license:zlib)))

(define-public imgp
  (package
    (name "imgp")
    (version "2.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "imgp" version))
       (sha256
        (base32 "0q99h9wv9rynig0s0flnr9mxi541zzl0gw8vh4y6m5x132diilri"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ;there are no tests
       #:phases
       (modify-phases %standard-phases
         ;; setup.py expects the file to be named 'imgp'.
         (add-after 'unpack 'rename-imgp
           (lambda _
             (rename-file "imgp.py" "imgp")
             #t)))))
    (inputs
     `(("python-pillow" ,python-pillow)))
    (home-page "https://github.com/jarun/imgp")
    (synopsis "High-performance CLI batch image resizer & rotator")
    (description
     "@code{imgp} is a command line image resizer and rotator for JPEG and PNG
images.  It can resize (or thumbnail) and rotate thousands of images in a go
while saving significantly on storage.

This package may optionally be built with @code{python-pillow-simd} in place
of @{python-pillow} for SIMD parallelism.")
    (license license:gpl3+)))

(define-public pngsuite
  (package
    (name "pngsuite")
    (version "2017jul19")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "http://www.schaik.com/pngsuite2011/PngSuite-"
                           version ".tgz"))
       (sha256
        (base32
         "1j7xgd9iffcnpphhzz9ld9ybrjmx9brhq0803g0450ssr52b5502"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; there is no test target
       #:license-file-regexp "PngSuite.LICENSE"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "." (string-append out "/"))
             #t)))
         (delete 'build)
         (delete 'configure))))
    (home-page "http://www.schaik.com/pngsuite2011/pngsuite.html")
    (synopsis "Example PNGs for use in test suites")
    (description "Collection of graphics images created to test PNG
applications like viewers, converters and editors.  As far as that is
possible, all formats supported by the PNG standard are represented.")
    (license (license:fsdg-compatible "file://PngSuite.LICENSE" "Permission to
use, copy, modify and distribute these images for any purpose and without fee
is hereby granted."))))

(define-public libjpeg-turbo
  (package
    (name "libjpeg-turbo")
    (version "2.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libjpeg-turbo/"
                                  version "/libjpeg-turbo-" version ".tar.gz"))
              (sha256
               (base32
                "0pbv6pc97kbj7ib31qcwi7lnmm9xg5y3b11aasmkhfjvf7rgdy0n"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("nasm" ,nasm)))
    (arguments
     `(#:configure-flags '("-DCMAKE_INSTALL_LIBDIR:PATH=lib"
                           "-DENABLE_STATIC=0"
                           ;; djpeg-shared-3x2-float-prog-cmp fails on 32-bit PPC.
                           ,@(if (string=? "powerpc-linux" (%current-system))
                               `("-DFLOATTEST=NO")
                               '())
                           ;; The build system probes for the current CPU, but
                           ;; that fails when cross-compiling.
                           ,@(let ((target (%current-target-system)))
                               (if target
                                   (cond
                                    ((string-prefix? "arm" target)
                                     `("-DCMAKE_SYSTEM_PROCESSOR=arm"))
                                    ((string-prefix? "aarch64" target)
                                     `("-DCMAKE_SYSTEM_PROCESSOR=aarch64"))
                                    ((string-prefix? "i686" target)
                                     `("-DCMAKE_SYSTEM_PROCESSOR=x86"))
                                    ((string-prefix? "x86_64" target)
                                     `("-DCMAKE_SYSTEM_PROCESSOR=x86_64"))
                                    ;; 32-bit and 64-bit
                                    ((string-prefix? "powerpc" target)
                                     `("-DCMAKE_SYSTEM_PROCESSOR=powerpc"))
                                    (else '()))
                                   '())))
       ,@(if (%current-target-system)
             '()
             ;; Use a special "bootstrap" CMake for the native build to work
             ;; around a circular dependency between CMake and this package.
             `(#:cmake ,cmake-bootstrap))))
    (home-page "https://libjpeg-turbo.org/")
    (synopsis "SIMD-accelerated JPEG image handling library")
    (description "libjpeg-turbo is a JPEG image codec that accelerates baseline
JPEG compression and decompression using SIMD instructions: MMX on x86, SSE2 on
x86-64, NEON on ARM, and AltiVec on PowerPC processors.  Even on other systems,
its highly-optimized Huffman coding routines allow it to outperform libjpeg by
a significant amount.
libjpeg-turbo implements both the traditional libjpeg API and the less powerful
but more straightforward TurboJPEG API, and provides a full-featured Java
interface.  It supports color space extensions that allow it to compress from
and decompress to 32-bit and big-endian pixel buffers (RGBX, XBGR, etc.).")
    ;; libjpeg-turbo is covered by three different licenses; see LICENSE.md.
    (license (list license:bsd-3        ;the TurboJPEG API library and programs
                   license:ijg          ;the libjpeg library and associated tools
                   license:zlib))))     ;the libjpeg-turbo SIMD extensions

(define-deprecated libjpeg libjpeg-turbo)
(export libjpeg)

(define-public niftilib
  (package
    (name "niftilib")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "mirror://sourceforge/niftilib/"
                                        "nifticlib/nifticlib_"
                                        (string-join (string-split version #\.) "_")
                                        "/nifticlib-" version ".tar.gz")))
              (sha256
               (base32 "123z9bwzgin5y8gi5ni8j217k7n683whjsvg0lrpii9flgk8isd3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; there is no test target
       #:parallel-build? #f             ; not supported
       #:make-flags
       (list "SHELL=bash"
             (string-append "ZLIB_INC="
                            (assoc-ref %build-inputs "zlib") "/include")
             ;; Append "-fPIC" to CFLAGS.
             (string-append "CFLAGS="
                            "-Wall -ansi -pedantic -fPIC"))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (dir)
                  (copy-recursively dir (string-append out "/" dir)))
                '("bin" "lib" "include")))
             #t))
         (delete 'configure))))
    (inputs
     `(("zlib" ,zlib)))
    (synopsis "Library for reading and writing files in the nifti-1 format")
    (description "Niftilib is a set of i/o libraries for reading and writing
files in the nifti-1 data format - a binary file format for storing
medical image data, e.g. magnetic resonance image (MRI) and functional MRI
(fMRI) brain images.")
    (home-page "http://niftilib.sourceforge.net")
    (license license:public-domain)))

(define-public gpick
  (package
    (name "gpick")
    (version "0.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/thezbyg/gpick")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nl89gca5nmbyycv5rl5bm6k7facapdk4pab9pl949aa3cjw9bk7"))))
    (build-system scons-build-system)
    (native-inputs
     `(("boost" ,boost)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("ragel" ,ragel)))
    (inputs
     `(("expat" ,expat)
       ("gtk2" ,gtk+-2)
       ("lua" ,lua-5.2)))
    (arguments
     `(#:tests? #f
       #:scons ,scons-python2
       #:scons-flags (list (string-append "DESTDIR=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-lua-reference
           (lambda _
             (substitute* "SConscript"
               (("lua5.2") "lua-5.2"))
             #t)))))
    (home-page "http://www.gpick.org/")
    (synopsis "Color picker")
    (description "Gpick is an advanced color picker and palette editing tool.")
    (license license:bsd-3)))

(define-public libiptcdata
  (package
    (name "libiptcdata")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ianw/libiptcdata"
                                  "/releases/download/release_"
                                  (string-join (string-split version #\.) "_")
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "17m2bscc76r1bymjgb44fbbfrdsjfqyb2ivg9wchyllm8pgx1560"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/ianw/libiptcdata")
    (synopsis "IPTC metadata manipulation library")
    (description
     "Libiptcdata is a C library for manipulating the International Press
Telecommunications Council (@dfn{IPTC}) metadata stored within multimedia files
such as images.  This metadata can include captions and keywords, often used by
popular photo management applications.  The library provides routines for
parsing, viewing, modifying, and saving this metadata.")
    (license license:lgpl2.0+)))

(define-public flameshot
  (package
    (name "flameshot")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flameshot-org/flameshot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1z77igs60lz106vsf6wsayxjafxm3llf2lm4dpvsqyyrxybfq191"))))
    (build-system qt-build-system)
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (arguments
     `(#:tests? #f))                    ;no tests
    (home-page "https://github.com/flameshot-org/flameshot")
    (synopsis "Powerful yet simple to use screenshot software")
    (description "Flameshot is a screenshot program.
Features:

@itemize
@item Customizable appearance.
@item Easy to use.
@item In-app screenshot edition.
@item DBus interface.
@item Upload to Imgur.
@end itemize\n")
    (license license:gpl3+)))

(define-public swappy
  (package
    (name "swappy")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jtheoof/swappy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14ac2jmnak7avcz0jhqjm30vk7pv3gq5aq5rdyh84k8c613kkicf"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("scdoc" ,scdoc)
       ("glib" ,glib "bin"))) ; for 'glib-compile-resources'
    (inputs
     `(("gtk+" ,gtk+)
       ("libnotify" ,libnotify)))
    (home-page "https://github.com/jtheoof/swappy")
    (synopsis "Grab and edit on the fly snapshots of a Wayland compositor")
    (description
     "@command{swappy} is a command-line utility to take and edit screenshots
of Wayland desktops.  Works great with grim, slurp and sway.  But can easily
work with other screen copy tools that can output a final PNG image to
stdout.")
    (license license:expat)))

(define-public gifsicle
  (package
   (name "gifsicle")
   (version "1.92")
   (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.lcdf.org/gifsicle/gifsicle-"
                           version ".tar.gz"))
       (sha256
        (base32 "0rffpzxcak19k6cngpxn73khvm3z1gswrqs90ycdzzb53p05ddas"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-before 'check 'patch-tests
          (lambda _
            (substitute* "test/testie"
              (("/usr/bin/perl")
               (which "perl"))
              (("/bin/sh")
               (which "sh"))
              (("/bin/rm")
               (which "rm")))
            #t)))))
   (native-inputs `(("perl" ,perl)))    ; only for tests
   (inputs `(("libx11" ,libx11)))
   (home-page "https://www.lcdf.org/gifsicle/")
   (synopsis "Edit GIF images and animations")
   (description "Gifsicle is a command-line GIF image manipulation tool that:

@itemize
@item Provides a batch mode for changing GIFs in place.
@item Prints detailed information about GIFs, including comments.
@item Control over interlacing, comments, looping, transparency, etc.
@item Creates well-behaved GIFs: removes redundant colors, only uses local color
tables, etc.
@item Shrinks colormaps and change images to use the Web-safe palette.
@item Optimizes GIF animations, or unoptimizes them for easier editing.
@end itemize

Two other programs are included with Gifsicle: @command{gifview} is a
lightweight animated-GIF viewer, and @command{gifdiff} compares two GIFs for
identical visual appearance.")
   (license license:gpl2+)))

;; 1.0.7 is buggy and reverted in git repository.
(define-public jp2a
  (package
    (name "jp2a")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://debian/pool/main/j/jp2a/jp2a_"
                           version ".orig.tar.gz"))
        (sha256
         (base32
          "076frk3pa16s4r1b10zgy81vdlz0385zh3ykbnkaij25jn5aqc09"))))
    (build-system gnu-build-system)
    (inputs
     `(("curl" ,curl)
       ("libjpeg" ,libjpeg-turbo)
       ("ncurses" ,ncurses)))
    (home-page "https://csl.name/jp2a/")
    (synopsis "Convert JPEG images to ASCII")
    (description
     "Jp2a is a small utility that converts JPEG images to ASCII.")
    (license license:gpl2)))

(define-public grim
  (package
   (name "grim")
   (version "1.3.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/emersion/grim")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0fjmjq0ws9rlblkcqxxw2lv7zvvyi618jqzlnz5z9zb477jwdfib"))))
   (build-system meson-build-system)
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("scdoc" ,scdoc)))
   (inputs `(("cairo" ,cairo)
             ("libjpeg-turbo" ,libjpeg-turbo)
             ("wayland" ,wayland)
             ("wayland-protocols" ,wayland-protocols)))
   (home-page "https://github.com/emersion/grim")
   (synopsis "Create screenshots from a Wayland compositor")
   (description "grim can create screenshots from a Wayland compositor.")
   ;; MIT license.
   (license license:expat)))

(define-public slurp
  (package
   (name "slurp")
   (version "1.3.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/emersion/slurp")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1fby2v2ylcadgclds05wpkl9xi2r9dfz49dqyqpn20rjv1wnz3jv"))))
   (build-system meson-build-system)
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("scdoc" ,scdoc)))
   (inputs
    `(("cairo" ,cairo)
      ("libxkbcommon" ,libxkbcommon)
      ("wayland" ,wayland)
      ("wayland-protocols" ,wayland-protocols)))
   (home-page "https://github.com/emersion/slurp")
   (synopsis "Select a region in a Wayland compositor")
   (description "Slurp can select a region in a Wayland compositor and print it
to the standard output.  It works well together with grim.")
   (license license:expat)))

(define-public sng
  (package
    (name "sng")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sng/sng-"
                           version ".tar.gz"))
       (sha256
        (base32 "06a6ydvx9xb3vxvrzdrg3hq0rjwwj9ibr7fyyxjxq6qx1j3mb70i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'link-pngsuite
           ;; tests expect pngsuite in source dir
           (lambda* (#:key inputs #:allow-other-keys)
             (symlink (assoc-ref inputs "pngsuite") "pngsuite")
             #t)))
       #:configure-flags
       (list (string-append "--with-rgbtxt="
                            (assoc-ref %build-inputs "xorg-rgb")
                            "/share/X11/rgb.txt"))))
    (inputs `(("xorg-rgb" ,xorg-rgb)
              ("libpng" ,libpng)))
    (native-inputs `(("pngsuite" ,pngsuite)))
    (home-page "http://sng.sourceforge.net")
    (synopsis "Markup language for representing PNG contents")
    (description "SNG (Scriptable Network Graphics) is a minilanguage designed
specifically to represent the entire contents of a PNG (Portable Network
Graphics) file in an editable form.  Thus, SNGs representing elaborate
graphics images and ancillary chunk data can be readily generated or modified
using only text tools.

SNG is implemented by a compiler/decompiler called sng that
losslessly translates between SNG and PNG.")
    (license license:zlib)))

(define-public lodepng
  ;; There are no tags in the repository, so we take the version as defined in
  ;; lodepng.cpp.
  (let ((commit "48e5364ef48ec2408f44c727657ac1b6703185f8")
        (revision "1")
        (version "20200215"))
    (package
      (name "lodepng")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lvandeve/lodepng")
                      (commit commit)))
                (sha256
                 (base32
                  "1a1x8ag2scanzb2066jm9hg2y9kaa3wmpgmz10l1x9bkpik612lw"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda _
               (setenv "CXXFLAGS" "-fPIC")
               (invoke "make" "lodepng.o")
               (invoke "make" "lodepng_util.o")
               (invoke "g++" "-fPIC" "-O3"
                       "-o" "liblodepng.so"
                       "-shared" "lodepng.o" "lodepng_util.o")
               #t))
           (replace 'check
             (lambda _
               (invoke "make" "unittest")
               (invoke "./unittest")
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (string-append out "/share/doc"))
                      (lib (string-append out "/lib"))
                      (include (string-append out "/include")))
                 (install-file "lodepng.h" include)
                 (install-file "lodepng_util.h" include)
                 (install-file "liblodepng.so" lib)
                 (install-file "README.md" doc)
                 #t))))))
      (home-page "https://lodev.org/lodepng/")
      (synopsis "PNG encoder and decoder in C and C++, without dependencies")
      (description "LodePNG is a PNG image decoder and encoder, all in one,
no dependency or linkage required.  It's made for C (ISO C90), and has a C++
wrapper with a more convenient interface on top.")
      (license license:zlib))))

(define-public icoutils
  (package
    (name "icoutils")
    (version "0.32.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://savannah/icoutils/icoutils-" version ".tar.bz2"))
       (sha256
        (base32 "1q66cksms4l62y0wizb8vfavhmf7kyfgcfkynil3n99s0hny1aqp"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("perl" ,perl)))
    (propagated-inputs
     `(("perl-libwww" ,perl-libwww)))
    (home-page "https://www.nongnu.org/icoutils/")
    (synopsis "Extract and convert bitmaps from Windows icon and cursor files")
    (description "Icoutils are a set of program for extracting and converting
bitmaps from Microsoft Windows icon and cursor files.  These files usually
have the extension @code{.ico} or @code{.cur}, but they can also be embedded
in executables and libraries (@code{.dll}-files).  (Such embedded files are
referred to as resources.)

Conversion of these files to and from PNG images is done @command{icotool}.
@command{extresso} automates these tasks with the help of special resource
scripts.  Resources such can be extracted from MS Windows executable and
library files with @command{wrestool}.

This package can be used to create @code{favicon.ico} files for web sites.")
     (license license:gpl3+)))

(define-public libavif
  (package
    (name "libavif")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/AOMediaCodec/libavif")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1aw41m8ddrckq375w0lv2zd4ybhccsy1hw4f9kipppwxhgvk17gf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DAVIF_CODEC_AOM=ON" "-DAVIF_CODEC_DAV1D=ON"
                           "-DAVIF_CODEC_RAV1E=ON"
                           "-DAVIF_BUILD_TESTS=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./aviftest" "../source/tests/data")))
         (add-after 'install 'install-readme
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/libavif-" ,version)))
               (install-file "../source/README.md" doc)
               #t))))))
    (inputs
     `(("dav1d" ,dav1d)
       ("libaom" ,libaom)
       ("rav1e" ,rav1e)))
    (synopsis "Encode and decode AVIF files")
    (description "Libavif is a C implementation of @acronym{AVIF, the AV1 Image
File Format}.  It can encode and decode all YUV formats and bit depths supported
by AOM, including with alpha.")
    (home-page "https://github.com/AOMediaCodec/libavif")
    (license (list license:bsd-2    ; libavif itself
                   license:expat)))) ; cJSON in the test suite

(define-public libheif
  (package
    (name "libheif")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/strukturag/libheif")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "036n63vlk4sk7y25q2kzyvvw4r5vv323ysbmbrcaprg9hdyjqgf5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;no test target although there is a tests folder
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dav1d" ,dav1d)
       ("gdk-pixbuf" ,gdk-pixbuf) ;optional
       ("libaom" ,libaom)
       ("libde265" ,libde265)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("x265" ,x265)))
    (home-page "https://github.com/strukturag/libheif")
    (synopsis "HEIF and AVIF file format decoder and encoder")
    (description
     "@code{libheif} is an ISO/IEC 23008-12:2017 HEIF and AVIF (AV1 Image File
Format) file format decoder and encoder.")
    (license license:lgpl3+)))

(define-public mtpaint
  (package
    (name "mtpaint")
    ;; The author neither releases tarballs nor uses git version tags.
    ;; Instead, author puts version in git commit title.
    (version "3.49.33")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wjaguar/mtPaint")
             (commit "5272e2b1e773c8e02ac3506b2d3bde82ad946b21")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bmq4m0dxczl18n1yiqb75g05a4c3pal1vdcyypkilx7ijsr0cmc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("imlib2" ,imlib2)
       ("libtiff" ,libtiff)
       ("libpng" ,libpng)
       ("libungif" ,libungif)
       ("libjpeg" ,libjpeg-turbo)
       ("libwebp" ,libwebp)
       ("openjpeg" ,openjpeg)
       ("lcms" ,lcms)
       ("zlib" ,zlib)
       ("glib" ,glib)
       ;; Support for gtk3 is in the testing stage.
       ("gtk+" ,gtk+-2)))
    (arguments
     `(#:configure-flags
       (list "intl"                     ; build internationalized version
             "man")                     ; build the man page
       #:tests? #f))                    ; no test suite
    (home-page "http://mtpaint.sourceforge.net/")
    (synopsis "Create pixel art and manipulate digital images")
    (description
     "Mtpaint is a graphic editing program which uses the GTK+ toolkit.
It can create and edit indexed palette or 24bit RGB images, offers basic
painting and palette manipulation tools.  It also handles JPEG, JPEG2000,
GIF, TIFF, WEBP, BMP, PNG, XPM formats.")
    (license license:gpl3+)))

(define-public mypaint
  (package
    (name "mypaint")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mypaint/mypaint/"
                                  "releases/download/v" version "/mypaint-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "05mvay73vb9d2sh1ckv4vny45n059dmsps1jcppjizfmrpbkgr7k"))))
    (build-system python-build-system)
    (arguments
     `(#:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gdk-pixbuf (assoc-ref inputs "gdk-pixbuf"))
                    (gtk+ (assoc-ref inputs "gtk+")))
               (wrap-program (string-append out "/bin/mypaint")
                 `("GI_TYPELIB_PATH" ":" prefix
                   (,(getenv "GI_TYPELIB_PATH"))))
               #t)))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests need writing access
             (setenv "HOME" "/tmp")
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("swig" ,swig)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("gtk+" ,gtk+)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("libmypaint" ,libmypaint)
       ("mypaint-brushes" ,mypaint-brushes)
       ("json-c" ,json-c)
       ("lcms" ,lcms)
       ("python-numpy" ,python-numpy)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)))
    (home-page "http://mypaint.org/")
    (synopsis "Fast and simple painting app for artists")
    (description
     "MyPaint is a simple drawing and painting program that works well with
Wacom-style graphics tablets.")
    (license license:gpl2+)))

(define-public phockup
  (package
    (name "phockup")
    (version "1.5.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ivandokov/phockup")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13ajj0xch7yfqaaxbw0awxs0fz17n1rxir4gqh2wcgxjysqk1j2y"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("src" "share/phockup/")
                        ("phockup.py" "share/phockup/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (list "src/dependency.py" "src/exif.py")
               (("exiftool")
                (string-append (assoc-ref inputs "perl-image-exiftool")
                               "/bin/exiftool")))
             #t))
         (add-before 'install 'check
           (lambda _
             (invoke "pytest")))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir (string-append out "/bin"))
               (symlink (string-append out "/share/phockup/phockup.py")
                        (string-append out "/bin/phockup")))
             #t)))))
    (inputs
     `(("perl-image-exiftool" ,perl-image-exiftool)
       ("python" ,python)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-mock" ,python-pytest-mock)))
    (home-page "https://github.com/ivandokov/phockup")
    (synopsis "Organize photos and videos in folders")
    (description "Phockup is a media sorting tool that uses creation date and
time information in photos and videos to organize them into folders by year,
month and day.  All files which are not images or videos or those which do not
have creation date information will be placed in a folder called
@file{unknown}.")
    (license license:expat)))
