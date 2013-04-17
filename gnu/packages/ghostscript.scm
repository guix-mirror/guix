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
             ("perl" ,perl)
             ("pkg-config" ,pkg-config) ; needed to find libtiff
             ("python" ,python)
             ("tcl" ,tcl)
             ("zlib" ,zlib)))
   (arguments
    `(#:phases
      (alist-replace
       'configure
       (lambda* (#:key #:allow-other-keys #:rest args)
        (let ((configure (assoc-ref %standard-phases 'configure)))
          (apply configure args)
          (substitute* "base/all-arch.mak"
            (("/bin/sh") (which "bash")))
          (substitute* "base/unixhead.mak"
            (("/bin/sh") (which "bash")))))
      (alist-replace
       'build
       (lambda* (#:key #:allow-other-keys #:rest args)
        (let ((build (assoc-ref %standard-phases 'build)))
          (apply build args)
          (system* "make" "so")))
      (alist-replace
       'install
       (lambda* (#:key #:allow-other-keys #:rest args)
        (let ((install (assoc-ref %standard-phases 'install)))
          (apply install args)
          (system* "make" "install-so")))
      %standard-phases)))))
   (synopsis "PostScript and PDF interpreter")
   (description
    "GNU Ghostscript is an interpreter for PostScript and Portable Document
Format (PDF) files.
It consists of a PostScript interpreter layer, and a graphics
library.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/ghostscript/")))
