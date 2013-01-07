;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages aspell)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (distro packages perl))

(define-public aspell
  (package
    (name "aspell")
    (version "0.60.6.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/aspell/aspell-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1qgn5psfyhbrnap275xjfrzppf5a83fb67gpql0kfqv37al869gm"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (home-page "http://aspell.net/")
    (synopsis
     "GNU Aspell, A spell checker for many languages")
    (description
     "GNU Aspell is a free spell checker designed to eventually replace
Ispell.  It can either be used as a library or as an independent spell
checker.  Its main feature is that it does a superior job of suggesting
possible replacements for a misspelled word than just about any other
spell checker out there for the English language.  Unlike Ispell, Aspell
can also easily check documents in UTF-8 without having to use a special
dictionary.  Aspell will also do its best to respect the current locale
setting.  Other advantages over Ispell include support for using
multiple dictionaries at once and intelligently handling personal
dictionaries when more than one Aspell process is open at once.")
    (license lgpl2.1+)))
