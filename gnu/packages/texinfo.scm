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
    (version "5.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/texinfo/texinfo-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0864v5i488x3mb3v5p6nhy2kw0mqkzpa3b0453iibj81zlpq078q"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("xz" ,xz)
              ("perl" ,perl)))
    (home-page "http://www.gnu.org/software/texinfo/")
    (synopsis "The GNU documentation format")
    (description
     "Texinfo is the official documentation format of the GNU project.
It was invented by Richard Stallman and Bob Chassell many years
ago, loosely based on Brian Reid's Scribe and other formatting
languages of the time.  It is used by many non-GNU projects as
well.

Texinfo uses a single source file to produce output in a number
of formats, both online and printed (dvi, html, info, pdf, xml,
etc.).  This means that instead of writing different documents
for online information and another for a printed manual, you
need write only one document.  And when the work is revised, you
need revise only that one document.  The Texinfo system is
well-integrated with GNU Emacs.")
    (license gpl3+)))
