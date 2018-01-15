;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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
    (version "20150325-3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://thrysoee.dk/editline"
                          "/" name "-" version ".tar.gz"))
      (sha256
       (base32
        "1if8zi9h52m80ck796an28rrqfljk2n8cn25m3fl0prwz155x2n8"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags (list "--enable-widec")))
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

;;; libedit.scm ends here
