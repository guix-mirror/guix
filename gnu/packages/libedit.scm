;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu packages libedit)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ncurses))

(define-public libedit
  (package
    (name "libedit")
    (version "20191231-3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://thrysoee.dk/editline"
                          "/libedit-" version ".tar.gz"))
      (sha256
       (base32 "0wch48nml28jj6ild889745dsg2agm7mpvrmbl1gi98nw6vjrf6v"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses))
    (home-page "https://thrysoee.dk/editline/")
    (synopsis "NetBSD Editline library")
    (description
     "This is an autotool- and libtoolized port of the NetBSD Editline
library (libedit).  This Berkeley-style licensed command line editor library
provides generic line editing, history, and tokenization functions, similar to
those found in GNU Readline.")
    (license bsd-3)))

(define-public editline
  (package
    (name "editline")
    (version "1.17.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://ftp.troglobit.com/editline/editline-" version ".tar.xz"))
      (sha256
       (base32 "03pw3z6pz590v8xfnjx0yynnzij2sb9xwjbvqvdzsid56crkn8nz"))))
    (build-system gnu-build-system)
    (home-page "https://troglobit.com/projects/editline/")
    (synopsis "Line editing library")
    (description
     "@code{editline} is a small line editing library.  It can be linked into almost
any program to provide command line editing and history functions.  It is call
compatible with the GNU Readline library, but at a fraction of the size,
and as a result fewer features.")
    (license bsd-4)))
