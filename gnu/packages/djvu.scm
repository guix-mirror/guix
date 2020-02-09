;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages djvu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public djvulibre
  (package
    (name "djvulibre")
    (version "3.5.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/djvu/DjVuLibre/"
                                  version "/djvulibre-" version ".tar.gz"))
              (sha256
               (base32
                "0psh3zl9dj4n4r3lx25390nx34xz0bg0ql48zdskhq354ljni5p6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'reproducible
                    (lambda _
                      ;; Ensure there are no timestamps in .svgz files.
                      (substitute* "desktopfiles/Makefile.in"
                        (("gzip") "gzip -n"))
                      #t)))))
    (home-page "http://djvu.sourceforge.net/")
    (synopsis "Implementation of DjVu, the document format")
    (description "DjVuLibre is an implementation of DjVu,
including viewers, browser plugins, decoders, simple encoders, and
utilities.")
    (license license:gpl2+)))

(define-public djview
  (package
    (name "djview")
    (version "4.10.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/djvu/DjView/"
                           (version-major+minor version) "/"
                           "djview-" version ".tar.gz"))
       (sha256
        (base32 "08bwv8ppdzhryfcnifgzgdilb12jcnivl4ig6hd44f12d76z6il4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("djvulibre" ,djvulibre)
       ("glib" ,glib)
       ("libxt" ,libxt)
       ("libtiff" ,libtiff)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-desktop-file
           ;; Executable is "djview", not "djview4".
           (lambda _
             (substitute* "desktopfiles/djvulibre-djview4.desktop"
               (("Exec=djview4 %f") "Exec=djview %f"))
             #t)))))
    (home-page "http://djvu.sourceforge.net/")
    (synopsis "Viewer for the DjVu image format")
    (description "DjView is a standalone viewer for DjVu files.

Its features include navigating documents, zooming and panning page images,
producing and displaying thumbnails, displaying document outlines, searching
documents for particular words in the hidden text layer, copying hidden text
to the clipboard, saving pages and documents as bundled or indirect multi-page
files, and printing page and documents.

The viewer can simultaneously display several pages using a side-by-side or
a continuous layout.")
    (license license:gpl2+)))
