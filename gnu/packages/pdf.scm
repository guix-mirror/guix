;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages curl)
  #:use-module (srfi srfi-1))

(define-public poppler
  (package
   (name "poppler")
   (version "0.28.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://poppler.freedesktop.org/poppler-"
                                version ".tar.xz"))
            (sha256 (base32
                     "01pxjdbhvpxf00ncf8d9wxc8gkcqcxz59lwrpa151ah988inxkrc"))))
   (build-system gnu-build-system)
   ;; FIXME: more dependencies could  be added
   ;;  cairo output:       no (requires cairo >= 1.10.0)
   ;;  qt4 wrapper:        no
   ;;    introspection:    no
   ;;  use gtk-doc:        no
   ;;  use libcurl:        no
   ;;  use libopenjpeg:    no
   (inputs `(("fontconfig" ,fontconfig)
             ("freetype" ,freetype)
             ("libjpeg-8" ,libjpeg-8)
             ("libpng" ,libpng)
             ("libtiff" ,libtiff)
             ("zlib" ,zlib)

             ;; To build poppler-glib (as needed by Evince), we need Cairo and
             ;; GLib.  But of course, that Cairo must not depend on Poppler.
             ("cairo" ,(package (inherit cairo)
                         (inputs (alist-delete "poppler"
                                               (package-inputs cairo)))))
             ("glib" ,glib)))
   (native-inputs
      `(("pkg-config" ,pkg-config)
        ("glib" ,glib "bin")))                    ; glib-mkenums, etc.
   (arguments
    `(#:tests? #f ; no test data provided with the tarball
      #:configure-flags
       '("--enable-xpdf-headers" ; to install header files
         "--enable-zlib")))
   (synopsis "PDF rendering library")
   (description
    "Poppler is a PDF rendering library based on the xpdf-3.0 code base.")
   (license license:gpl2+)
   (home-page "http://poppler.freedesktop.org/")))

(define-public xpdf
  (package
   (name "xpdf")
   (version "3.04")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.foolabs.com/pub/xpdf/xpdf-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1rbp54mr3z2x3a3a1qmz8byzygzi223vckfam9ib5g1sfds0qf8i"))))
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
             ("libpng" ,libpng)
             ("zlib" ,zlib)))
   (arguments
    `(#:tests? #f ; there is no check target
      #:parallel-build? #f            ; build fails randomly on 8-way machines
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
   (synopsis "Viewer for PDF files based on the Motif toolkit")
   (description
    "Xpdf is a viewer for Portable Document Format (PDF) files")
   (license license:gpl3) ; or gpl2, but not gpl2+
   (home-page "http://www.foolabs.com/xpdf/")))

(define-public podofo
  (package
    (name "podofo")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/podofo/podofo-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1wx3s0718rmhdzdwyi8hgpf2s92sk3hijy8f4glrmnjpiihr2la6"))))
    (build-system cmake-build-system)
    (inputs                                      ; TODO: Add cppunit for tests
     `(;; Optional Lua support fails to build with:
       ;;   error: 'luaL_getn' was not declared in this scope
       ;; ("lua" ,lua)
       ("libpng" ,libpng)
       ("openssl" ,openssl)
       ("fontconfig" ,fontconfig)
       ("libtiff" ,libtiff)
       ("libjpeg" ,libjpeg-8)
       ("freetype" ,freetype)
       ("zlib" ,zlib)))
    (home-page "http://podofo.sourceforge.net")
    (synopsis "Tools to work with the PDF file format")
    (description
     "PoDoFo is a C++ library and set of command-line tools to work with the
PDF file format.  It can parse PDF files and load them into memory, and makes
it easy to modify them and write the changes to disk.  It is primarily useful
for applications that wish to do lower level manipulation of PDF, such as
extracting content or merging files.")
    (license license:lgpl2.0+)))

(define-public mupdf
  (package
    (name "mupdf")
    (version "1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://mupdf.com/downloads/archive/"
                            name "-" version "-source.tar.gz"))
        (sha256
          (base32 "0qx51rj6alzcagcixm59rvdpm54w6syrwr4184v439jh14ryw4wq"))
        (patches
          (list (search-patch "mupdf-buildsystem-fix.patch")))
        (modules '((guix build utils)))
        (snippet
          '(begin
            ;; Don't build the bundled-in third party libraries.
            (delete-file-recursively "thirdparty")

            ;; Make the scripts for finding openjpeg build details executable.
            (chmod "ojp2_cppflags.sh" #o0755)
            (chmod "ojp2_ldflags.sh" #o0755)))))

    (build-system gnu-build-system)
    (inputs
      `(("curl" ,curl)
        ("freetype" ,freetype)
        ("jbig2dec" ,jbig2dec)
        ("libjpeg" ,libjpeg)
        ("libx11" ,libx11)
        ("libxext" ,libxext)
        ("openjpeg" ,openjpeg)
        ("openssl" ,openssl)
        ("zlib" ,zlib)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      ;; Trying to run `$ make check' results in a no rule fault.
      '(#:tests? #f

        #:modules ((guix build gnu-build-system)
                     (guix build utils)
                     (srfi srfi-1))
        #:phases (alist-replace
                   'build
                   (lambda _ (zero? (system* "make" "XCFLAGS=-fpic")))
                   (alist-replace
                     'install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (zero? (system* "make" (string-append "prefix=" out)
                                         "install"))))
                     (alist-delete 'configure %standard-phases)))))
    (home-page "http://mupdf.com")
    (synopsis "Lightweight PDF viewer and toolkit")
    (description
      "MuPDF is a C library that implements a PDF and XPS parsing and
rendering engine.  It is used primarily to render pages into bitmaps,
but also provides support for other operations such as searching and
listing the table of contents and hyperlinks.

The library ships with a rudimentary X11 viewer, and a set of command
line tools for batch rendering (pdfdraw), examining the file structure
(pdfshow), and rewriting files (pdfclean).")
    (license license:agpl3+)))
