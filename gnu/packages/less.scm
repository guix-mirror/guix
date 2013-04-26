;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages less)
  #:use-module (guix licenses)
  #:use-module (gnu packages ncurses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public less
  (package
    (name "less")
    (version "451")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/less/less-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0mszdd9m1dsbg59pav62swg9f87xmjpfspcw2jsazzksciy2is4z"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://www.gnu.org/software/less/")
    (synopsis "Paginator for terminals")
    (description
     "GNU less is a program similar to more, but which allows backward
movement in the file as well as forward movement. Also, less does not
have to read the entire input file before starting, so with large input
files it starts up faster than text editors like vi. Less uses
termcap (or terminfo on some systems), so it can run on a variety of
terminals. There is even limited support for hardcopy terminals.")
    (license gpl3+))) ; some files are under GPLv2+
