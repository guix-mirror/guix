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

(define-module (gnu packages ghostscript)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tcl)
  #:use-module ((guix licenses) #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public lcms
  (package
   (name "lcms")
   (version "2.4")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://downloads.sourceforge.net/project/lcms/lcms/"
                   version "/lcms2-" version ".tar.gz"))
            (sha256 (base32
                     "1s1ppvqaydf2yqc72mw6zfviwxccb311a6hrbi802sgjxw84sl9a"))))
   (build-system gnu-build-system)
   (inputs `(("libjpeg-8" ,libjpeg-8)
             ("libtiff" ,libtiff)
             ("zlib" ,zlib)))
   (synopsis "Little CMS, a small-footprint colour management engine")
   (description
    "Little CMS is a small-footprint colour management engine, with special
focus on accuracy and performance. It uses the International Color
Consortium standard (ICC), approved as ISO 15076-1.")
   (license license:x11)
   (home-page "http://www.littlecms.com/")))

(define-public libpaper
  (package
   (name "libpaper")
   (version "1.1.24")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://ftp.de.debian.org/debian/pool/main/libp/libpaper/libpaper_"
                   version ".tar.gz"))
            (sha256 (base32
                     "0zhcx67afb6b5r936w5jmaydj3ks8zh83n9rm5sv3m3k8q8jib1q"))))
   (build-system gnu-build-system)
   (synopsis "libpaper, a library for handling paper sizes")
   (description
    "The paper library and accompanying files are intended to provide a simple
way for applications to take actions based on a system- or user-specified
paper size.")
   (license license:gpl2)
   (home-page "http://packages.qa.debian.org/libp/libpaper.html")))

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
      (alist-replace
       'configure
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
           ((" install.include") ""))))
      %standard-phases)))
   (synopsis "psutils, a collection of utilities for manipulating PostScript documents")
   (description
    "PSUtils is a collection of utilities for manipulating PostScript
documents. Programs included are psnup, for placing out several logical pages
on a single sheet of paper, psselect, for selecting pages from a document,
pstops, for general imposition, psbook, for signature generation for booklet
printing, and psresize, for adjusting page sizes.")
   (license (license:bsd-style "file://LICENSE"
                                "See LICENSE in the distribution."))
   (home-page "http://knackered.org/angus/psutils/")))

(define-public ghostscript
  (package
   (name "ghostscript")
   (version "9.06.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/ghostscript/gnu-ghostscript-"
                                version ".tar.xz"))
            (sha256 (base32
                     "0bcg2203p7cm0f53f3s883xhj2c91xnaxakj2cy7kcdknfxplvs4"))))
   (build-system gnu-build-system)
   (inputs `(("freetype" ,freetype)
             ("lcms" ,lcms)
             ("libjpeg-8" ,libjpeg-8)
             ("libpng" ,libpng)
             ("libpaper" ,libpaper)
             ("libtiff" ,libtiff)
             ("zlib" ,zlib)))
   (native-inputs
      `(("perl" ,perl)
        ("pkg-config" ,pkg-config) ; needed to find libtiff
        ("python" ,python-wrapper)
        ("tcl" ,tcl)))
   (arguments
    `(#:phases
      (alist-cons-after
       'configure 'patch-config-files
       (lambda _
         (substitute* "base/all-arch.mak"
           (("/bin/sh") (which "bash")))
         (substitute* "base/unixhead.mak"
           (("/bin/sh") (which "bash"))))
      (alist-cons-after
       'build 'build-so
       (lambda _ (system* "make" "so"))
      (alist-cons-after
       'install 'install-so
       (lambda _ (system* "make" "install-so"))
      %standard-phases)))))
   (synopsis "PostScript and PDF interpreter")
   (description
    "Ghostscript is an interpreter for the PostScript language and the PDF
file format.  It also includes a C library that implements the graphics
capabilities of the PostScript language.  It supports a wide variety of
output file formats and printers.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/ghostscript/")))

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
                 (srfi srfi-1)) ; for alist-delete
      #:phases
       (alist-delete
        'configure
       (alist-delete
        'build
       (alist-replace
        'install
        (lambda* (#:key outputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (dir (string-append out "/share/fonts/type1/ghostscript")))
            (mkdir-p dir)
            (for-each
              (lambda (file)
                (copy-file file (string-append dir "/" file)))
              (find-files "." "pfb|afm"))))
       %standard-phases)))))
   (synopsis "free replacements for the PostScript fonts")
   (description
    "gs-fonts provides fonts and font metrics customarily distributed with
Ghostscript. It currently includes the 35 standard PostScript fonts.")
   (license license:gpl2)
   (home-page "http://sourceforge.net/projects/gs-fonts/")))

(define-public libspectre
  (package
   (name "libspectre")
   (version "0.2.7")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://libspectre.freedesktop.org/releases/libspectre-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1v63lqc6bhhxwkpa43qmz8phqs8ci4dhzizyy16d3vkb20m846z8"))))
   (build-system gnu-build-system)
   (inputs `(("ghostscript" ,ghostscript)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (synopsis "postscript rendering library")
   (description
    "libspectre is a small library for rendering Postscript documents.
It provides a convenient easy to use API for handling and rendering
Postscript documents.")
   (license license:gpl2+)
   (home-page "http://www.freedesktop.org/wiki/Software/libspectre")))
