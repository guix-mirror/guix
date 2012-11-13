;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages texinfo)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (distro packages compression)
  #:use-module (distro packages ncurses))

(define-public texinfo
  (package
    (name "texinfo")
    (version "4.13a")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnu/texinfo/texinfo-"
            version
            ".tar.lzma"))
      (sha256
       (base32
        "1rf9ckpqwixj65bw469i634897xwlgkm5i9g2hv3avl6mv7b0a3d"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses) ("xz" ,xz)))
    (home-page
     "http://www.gnu.org/software/texinfo/")
    (synopsis
     "GNU Texinfo, the GNU documentation system")
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
    (license "GPLv3+")))
