;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages asciidoc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:autoload   (gnu packages zip) (unzip))

(define-public asciidoc
  (package
    (name "asciidoc")
    (version "8.6.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/asciidoc/asciidoc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1w71nk527lq504njmaf0vzr93pgahkgzzxzglrq6bay8cw2rvnvq"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))                    ; no 'check' target
    (inputs `(("python" ,python-2)))
    (home-page "http://www.methods.co.nz/asciidoc/")
    (synopsis "Text-based document generation system")
    (description
     "AsciiDoc is a text document format for writing notes, documentation,
articles, books, ebooks, slideshows, web pages, man pages and blogs.
AsciiDoc files can be translated to many formats including HTML, PDF,
EPUB, man page.

AsciiDoc is highly configurable: both the AsciiDoc source file syntax and
the backend output markups (which can be almost any type of SGML/XML
markup) can be customized and extended by the user.")
    (license gpl2+)))
