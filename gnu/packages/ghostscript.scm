;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2013, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages ghostscript)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public lcms
  (package
   (name "lcms")
   (version "2.9")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/lcms/lcms/" version
                                "/lcms2-" version ".tar.gz"))

            (patches (search-patches "lcms-CVE-2018-16435.patch"))
            (sha256 (base32
                     "083xisy6z01zhm7p7rgk4bx9d6zlr8l20qkfv1g29ylnhgwzvij8"))))
   (build-system gnu-build-system)
   (inputs `(("libjpeg" ,libjpeg)
             ("libtiff" ,libtiff)
             ("zlib" ,zlib)))
   (synopsis "Little CMS, a small-footprint colour management engine")
   (description
    "Little CMS is a small-footprint colour management engine, with special
focus on accuracy and performance.  It uses the International Color
Consortium standard (ICC), approved as ISO 15076-1.")
   (license license:x11)
   (home-page "http://www.littlecms.com/")
   (properties '((cpe-name . "little_cms_color_engine")))))

(define-public libpaper
  (package
   (name "libpaper")
   (version "1.1.24")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   ;; Debian moved their libpaper-1.1.24 to archive.debian.net
                   ;; but in the move the hash of their tarball changed.
                   "http://pkgs.fedoraproject.org/repo/pkgs/libpaper/libpaper_"
                   version ".tar.gz/5bc87d494ba470aba54f6d2d51471834/libpaper_"
                   version ".tar.gz"))
            (sha256 (base32
                     "0zhcx67afb6b5r936w5jmaydj3ks8zh83n9rm5sv3m3k8q8jib1q"))))
   (build-system gnu-build-system)
   (synopsis "Library for handling paper sizes")
   (description
    "The paper library and accompanying files are intended to provide a simple
way for applications to take actions based on a system- or user-specified
paper size.")
   (license license:gpl2)
   (home-page "https://packages.qa.debian.org/libp/libpaper.html")))

(define-public psutils
  (package
   (name "psutils")
   (version "17")
   (source (origin
            (method url-fetch)
            (uri "ftp://ftp.knackered.org/pub/psutils/psutils.tar.gz")
            (sha256 (base32
                     "1r4ab1fvgganm02kmm70b2r1azwzbav2am41gbigpa2bb1wynlrq"))))
   (build-system gnu-build-system)
   (inputs `(("perl" ,perl)))
   (arguments
    `(#:tests? #f ; none provided
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
           (let ((perl (assoc-ref inputs "perl"))
                 (out (assoc-ref outputs "out")))
            (copy-file "Makefile.unix" "Makefile")
            (substitute* "Makefile"
              (("/usr/local/bin/perl") (string-append perl "/bin/perl")))
            (substitute* "Makefile"
              (("/usr/local") out))
            ;; for the install phase
            (substitute* "Makefile"
              (("-mkdir") "mkdir -p"))
            ;; drop installation of non-free files
            (substitute* "Makefile"
              ((" install.include") "")))
           #t)))))
   (synopsis "Collection of utilities for manipulating PostScript documents")
   (description
    "PSUtils is a collection of utilities for manipulating PostScript
documents.  Programs included are psnup, for placing out several logical pages
on a single sheet of paper, psselect, for selecting pages from a document,
pstops, for general imposition, psbook, for signature generation for booklet
printing, and psresize, for adjusting page sizes.")
   (license (license:non-copyleft "file://LICENSE"
                                "See LICENSE in the distribution."))
   (home-page "http://knackered.org/angus/psutils/")))

(define-public ghostscript
  (package
    (name "ghostscript")
    (version "9.26")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ArtifexSoftware/"
                            "ghostpdl-downloads/releases/download/gs"
                            (string-delete #\. version)
                            "/ghostscript-" version ".tar.xz"))
        (sha256
         (base32
          "1645f47all5w27bfhiq15vycdm954lmr6agqkrp68ksq6xglgvch"))
        (patches (search-patches "ghostscript-no-header-creationdate.patch"
                                 "ghostscript-no-header-id.patch"
                                 "ghostscript-no-header-uuid.patch"))
        (modules '((guix build utils)))
        (snippet
          ;; Remove bundled libraries. The bundled OpenJPEG is a patched fork so
          ;; we leave it, at least for now.
          ;; TODO Try unbundling ijs, which is developed alongside Ghostscript.
          ;; Likewise for the thread-safe lcms2 fork called "lcms2art".
         '(begin
            (for-each delete-file-recursively '("freetype" "jbig2dec" "jpeg"
                                                "libpng" "tiff" "zlib"))
            #t))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))                  ;19 MiB of HTML/PS doc + examples
    (arguments
     `(#:disallowed-references ("doc")
       #:configure-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")
             "--with-system-libtiff"
             "LIBS=-lz"
             (string-append "ZLIBDIR="
                            (assoc-ref %build-inputs "zlib") "/include")
             "--enable-dynamic"

             ,@(if (%current-target-system)
                   '(;; Specify the native compiler, which is used to build 'echogs'
                     ;; and other intermediary tools when cross-compiling; see
                     ;; <https://ghostscript.com/FAQ.html>.
                     "CCAUX=gcc"

                     ;; Save 'config.log' etc. of the native build under
                     ;; auxtmp/, useful for debugging.
                     "--enable-save_confaux")
                   '()))
       #:phases
       (modify-phases %standard-phases
        (add-before 'configure 'create-output-directory
          (lambda* (#:key outputs #:allow-other-keys)
            ;; The configure script refuses to function if the directory
            ;; specified as -rpath does not already exist.
            (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
            #t))
        (add-after 'configure 'remove-doc-reference
          (lambda _
            ;; Don't retain a reference to the 'doc' output in 'gs'.
            ;; The only use of this definition is in the output of
            ;; 'gs --help', so this change is fine.
            (substitute* "base/gscdef.c"
              (("GS_DOCDIR")
               "\"~/.guix-profile/share/doc/ghostscript\""))
            ;; The docdir default changed in 9.23 and a compatibility
            ;; symlink was added from datadir->docdir.  Remove it.
            (substitute* "base/unixinst.mak"
              (("ln -s \\$\\(DESTDIR\\)\\$\\(docdir\\).*") ""))
            #t))
         (add-after 'configure 'patch-config-files
           (lambda _
             (substitute* "base/unixhead.mak"
               (("/bin/sh") (which "sh")))
             #t))
         ,@(if (%current-target-system)
               `((add-after 'configure 'add-native-lz
                   (lambda _
                     ;; Add missing '-lz' for native tools such as 'mkromfs'.
                     (substitute* "Makefile"
                       (("^AUXEXTRALIBS=(.*)$" _ value)
                        (string-append "AUXEXTRALIBS = -lz " value "\n")))
                     #t)))
               '())
         (replace 'build
           (lambda _
             ;; Build 'libgs.so', but don't build the statically-linked 'gs'
             ;; binary (saves 22 MiB).
             (invoke "make" "so" "-j"
                     (number->string (parallel-job-count)))))
         (replace 'install
           (lambda _
             (invoke "make" "soinstall")))
         (add-after 'install 'create-gs-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Some programs depend on having a 'gs' binary available.
               (symlink "gsc" (string-append out "/bin/gs"))
               #t))))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-wrapper)
       ("tcl" ,tcl)

       ;; When cross-compiling, some of the natively-built tools require all
       ;; these libraries.
       ,@(if (%current-target-system)
             `(("zlib/native" ,zlib)
               ("libjpeg/native" ,libjpeg))
             '())))
    (inputs
     `(("freetype" ,freetype)
       ("jbig2dec" ,jbig2dec)
       ("libjpeg" ,libjpeg)
       ("libpaper" ,libpaper)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("zlib" ,zlib)))
    (synopsis "PostScript and PDF interpreter")
    (description
     "Ghostscript is an interpreter for the PostScript language and the PDF
file format.  It also includes a C library that implements the graphics
capabilities of the PostScript language.  It supports a wide variety of
output file formats and printers.")
    (home-page "https://www.ghostscript.com/")
    (license license:agpl3+)))

(define-public ghostscript/x
  (package/inherit ghostscript
    (name (string-append (package-name ghostscript) "-with-x"))
    (inputs `(("libxext" ,libxext)
              ("libxt" ,libxt)
              ,@(package-inputs ghostscript)))))

(define-public ghostscript/cups
  (package/inherit ghostscript
    (name "ghostscript-with-cups")
    (inputs `(("cups" ,cups-minimal)
              ,@(package-inputs ghostscript)))))

(define-public ijs
  (package
   (name "ijs")
   (version (package-version ghostscript))
   (source (package-source ghostscript))
   (build-system gnu-build-system)
   (native-inputs
    `(("libtool"    ,libtool)
      ("automake"   ,automake)
      ("autoconf"   ,autoconf)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'autogen
          (lambda _
            ;; need to regenerate macros
            (system* "autoreconf" "-if")
            ;; do not run configure
            (substitute* "autogen.sh"
              (("^.*\\$srcdir/configure.*") ""))
            (system* "bash" "autogen.sh")
            ;; create configure script in ./ijs/
            (chdir "ijs")
            ;; do not run configure
            (substitute* "autogen.sh"
              (("^.*\\$srcdir/configure.*") "")
              (("^ + && echo Now type.*$")  ""))
            (invoke "bash" "autogen.sh"))))))
   (synopsis "IJS driver framework for inkjet and other raster devices")
   (description
    "IJS is a protocol for transmission of raster page images.  This package
provides the reference implementation of the raster printer driver
architecture.")
   (license license:expat)
   (home-page (package-home-page ghostscript))))

(define-public gs-fonts
  (package
   (name "gs-fonts")
   (version "8.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/gs-fonts/gs-fonts/"
                                version
                                "%20%28base%2035%2C%20GPL%29/ghostscript-fonts-std-"
                                version
                                ".tar.gz"))
            (sha256 (base32
                     "00f4l10xd826kak51wsmaz69szzm2wp8a41jasr4jblz25bg7dhf"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; nothing to check, just files to copy

      #:modules ((guix build gnu-build-system)
                 (guix build utils)
                 (srfi srfi-1))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (delete 'build)
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (dir (string-append out "/share/fonts/type1/ghostscript")))
              (mkdir-p dir)
              (for-each
                (lambda (file)
                  (copy-file file (string-append dir "/" file)))
                (find-files "." "pfb|afm"))
              #t))))))
   (synopsis "Free replacements for the PostScript fonts")
   (description
    "Ghostscript fonts provides fonts and font metrics customarily distributed with
Ghostscript.  It currently includes the 35 standard PostScript fonts.")
   (license license:gpl2)
   (home-page "https://sourceforge.net/projects/gs-fonts/")))

(define-public libspectre
  (package
   (name "libspectre")
   (version "0.2.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://libspectre.freedesktop.org/releases/libspectre-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1a67iglsc3r05mzngyg9kb1gy8whq4fgsnyjwi7bqfw2i7rnl9b5"))))
   (build-system gnu-build-system)
   (inputs `(("ghostscript" ,ghostscript)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (synopsis "Postscript rendering library")
   (description
    "libspectre is a small library for rendering Postscript documents.
It provides a convenient easy to use API for handling and rendering
Postscript documents.")
   (license license:gpl2+)
   (home-page "https://www.freedesktop.org/wiki/Software/libspectre")))
