;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
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

(define-module (gnu packages wv)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:renamer (symbol-prefix-proc 'license:)))

(define-public wv
  (package
    (name "wv")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append
            "http://sourceforge.net/projects/" name "/files/wv/" version
            "/wv-" version ".tar.gz/download"))
        (file-name (string-append "wv-" version ".tar.gz"))
        (sha256
          (base32 "1mn2ax6qjy3pvixlnvbkn6ymy6y4l2wxrr4brjaczm121s8hjcb7"))))

    (build-system gnu-build-system)
    (arguments
      `(#:configure-flags '("--with-libwmf")))
    (inputs
      `(("glib" ,glib)
        ("libgsf" ,libgsf)
        ("libjpeg" ,libjpeg)
        ("libpng" ,libpng)
        ("libwmf" ,libwmf)
        ("zlib" ,zlib)))
    (native-inputs
      `(("glib" ,glib "bin")
        ("pkg-config" ,pkg-config)))
    (synopsis "Microsoft Word conversion library and utilities")
    (description
      "wv converts Word 2,6,7,8,9 files to HTML and LaTeX.  The Word 2
conversion is still incomplete (no formatting), but it will do a passable job
extracting the text, which is what you probably want anyway.

libwv can be used as a library by third party programs, AbiWord uses it as its
word importer, and KWord may use it in the future.")
    (home-page "http://wvware.sourceforge.net/")
    (license license:gpl2+)))
