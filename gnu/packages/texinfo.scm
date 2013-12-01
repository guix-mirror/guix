;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages texinfo)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses))

(define-public texinfo
  (package
    (name "texinfo")
    (version "5.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/texinfo/texinfo-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1njfwh2z34r2c4r0iqa7v24wmjzvsfyz4vplzry8ln3479lfywal"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("xz" ,xz)
              ("perl" ,perl)))
    (home-page "http://www.gnu.org/software/texinfo/")
    (synopsis "The GNU documentation format")
    (description
     "Texinfo is the official documentation format of the GNU project.  It
uses a single source file using explicit commands to produce a final document
in any of several supported output formats, such as HTML or PDF.  This
package includes both the tools necessary to produce Info documents from
their source and the command-line Info reader.  The emphasis of the language
is on expressing the content semantically, avoiding physical markup commands.")
    (license gpl3+)))
