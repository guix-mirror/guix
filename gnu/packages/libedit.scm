;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
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
    (version "20190324-3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://thrysoee.dk/editline"
                          "/libedit-" version ".tar.gz"))
      (sha256
       (base32 "1bhvp8xkkgrg89k4ci1k8vjl3nhb6szd4ghy9lp4jrfgq58hz3xc"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://thrysoee.dk/editline/")
    (synopsis "NetBSD Editline library")
    (description
     "This is an autotool- and libtoolized port of the NetBSD Editline
library (libedit).  This Berkeley-style licensed command line editor library
provides generic line editing, history, and tokenization functions, similar to
those found in GNU Readline.")
    (license bsd-3)))
