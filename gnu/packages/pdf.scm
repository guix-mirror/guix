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

(define-module (gnu packages pdf)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public poppler
  (package
   (name "poppler")
   (version "0.22.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://poppler.freedesktop.org/poppler-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1rmrspavldlpqi6g76fijcmshy80m0kxd01nc1dmy4id3h4las44"))))
   (build-system gnu-build-system)
   ;; FIXME: more dependencies could  be added
   ;;  cairo output:       no (requires cairo >= 1.10.0)
   ;;  qt4 wrapper:        no
   ;;  glib wrapper:       no (requires cairo output)
   ;;    introspection:    no
   ;;  use gtk-doc:        no
   ;;  use libcurl:        no
   ;;  use libopenjpeg:    no
   (inputs `(("fontconfig" ,fontconfig)
             ("freetype" ,freetype)
             ("libjpeg-8" ,libjpeg-8)
             ("libpng" ,libpng)
             ("libtiff" ,libtiff)
             ("pkg-config" ,pkg-config)
             ("zlib" ,zlib)))
   (arguments
    `(#:tests? #f ; no test data provided with the tarball
      #:configure-flags
       '("--enable-xpdf-headers" ; to install header files
         "--enable-zlib")))
   (synopsis "Poppler, a pdf rendering library")
   (description
    "Poppler is a PDF rendering library based on the xpdf-3.0 code base.")
   (license license:gpl2+)
   (home-page "http://poppler.freedesktop.org/")))

(define-public xpdf
  (package
   (name "xpdf")
   (version "3.03")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.foolabs.com/pub/xpdf/xpdf-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1jnfzdqc54wa73lw28kjv0m7120mksb0zkcn81jdlvijyvc67kq2"))
            (patches (list (search-patch "xpdf-constchar.patch")))))
   (build-system gnu-build-system)
   (inputs `(("freetype" ,freetype)
             ("gs-fonts" ,gs-fonts)
             ("lesstif" ,lesstif)
             ("libpaper" ,libpaper)
             ("libx11" ,libx11)
             ("libxext" ,libxext)
             ("libxp" ,libxp)
             ("libxpm" ,libxpm)
             ("libxt" ,libxt)
             ("zlib" ,zlib)))
   (arguments
    `(#:tests? #f ; there is no check target
      #:phases
       (alist-replace
        'install
        (lambda* (#:key outputs inputs #:allow-other-keys #:rest args)
         (let* ((install (assoc-ref %standard-phases 'install))
                (out (assoc-ref outputs "out"))
                (xpdfrc (string-append out "/etc/xpdfrc"))
                (gs-fonts (assoc-ref inputs "gs-fonts")))
               (apply install args)
               (substitute* xpdfrc
                (("/usr/local/share/ghostscript/fonts")
                 (string-append gs-fonts "/share/fonts/type1/ghostscript"))
                (("#fontFile") "fontFile"))))
        %standard-phases)))
   (synopsis "Viewer for pdf files based on the Motif toolkit.")
   (description
    "Xpdf is a viewer for Portable Document Format (PDF) files")
   (license license:gpl3) ; or gpl2, but not gpl2+
   (home-page "http://www.foolabs.com/xpdf/")))
