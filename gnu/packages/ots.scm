;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages ots)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ots
  (package
    (name "ots")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       ;; libots seems to have left sourceforge and taken their release
       ;; tarballs with them
       (uri (list (string-append "mirror://debian/pool/main/o/ots/ots_"
                                 version ".orig.tar.gz")
                  (string-append "mirror://sourceforge/libots/libots/"
                                 name "-" version "/" name "-" version
                                 ".tar.gz")))
       (sha256
        (base32 "0dz1ccd7ymzk4swz1aly4im0k3pascnshmgg1whd2rk14li8v47a"))
       (patches (search-patches "ots-no-include-missing-file.patch"))))

    (build-system gnu-build-system)
    (arguments
     ;; With '-jN', the rule to build the 'ots' command can be triggered
     ;; before libots-1.la has been built.
     '(#:parallel-build? #f

       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'set-shared-lib-extension
           (lambda _
             ;; For some reason, the 'libtool' script (from Libtool
             ;; 1.5.2, Debian variant) sets 'shrext_cmds' instead of
             ;; 'shrext' for the shared library file name extension.
             ;; This leads to the creation of 'libots-1' instead of
             ;; 'libots-1.so'.  Fix that.
             (substitute* "libtool"
               (("shrext_cmds") "shrext"))
             #t)))))
    (inputs
      (list glib popt libxml2 zlib))
    (native-inputs
      `(("glib:bin" ,glib "bin")
        ("pkg-config" ,pkg-config)))
    (synopsis "Tool for summarizing texts")
    (description
      "The Open Text Summarizer is a library and command-line tool for
summarizing texts.  The program reads a text and decides which sentences are
important and which are not.  OTS will create a short summary or will
highlight the main ideas in the text.

The program can either print the summarized text as plain text or HTML.  If in
HTML, the important sentences are highlighted.

The program is multi lingual and works with UTF-8 encoding.")
    (home-page "http://libots.sourceforge.net/")
    (license license:gpl2+)))
