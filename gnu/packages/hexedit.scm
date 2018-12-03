;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages hexedit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public hexedit
  (package
    (name "hexedit")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pixel/hexedit.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xsxa5mip892jkvz9jshj73y6c7j3mgp8y393ciihqlyf2nmfs67"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))          ; no check target
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("ncurses" ,ncurses)))
    (synopsis "View and edit files or devices in hexadecimal or ASCII")
    (description "hexedit shows a file both in ASCII and in hexadecimal.  The
file can be a device as the file is read a piece at a time.  You can modify
the file and search through it.")
    (home-page "http://rigaux.org/hexedit.html")
    (license license:gpl2+)))

(define-public ht
  (package
    (name "ht")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://sourceforge.net/projects/hte/files/ht-source/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0w2xnw3z9ws9qrdpb80q55h6ynhh3aziixcfn45x91bzrbifix9i"))))
    (build-system gnu-build-system)
    (inputs
     `(("lzo" ,lzo)
       ("ncurses" ,ncurses)))
    (synopsis "Viewer, editor, and analyzer for executable binaries")
    (description
     "ht is a terminal-based program to view, edit, and analyze any file, but
with a special focus on executable binaries.  Its goal is to combine the
low-level functionality of a debugger with the usability of an @dfn{Integrated
Development Environment} (IDE).")
    (home-page "http://hte.sourceforge.net/")
    (license license:gpl2)))
