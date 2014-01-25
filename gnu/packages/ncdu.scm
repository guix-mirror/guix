;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014  John Darrington <jmd@gnu.org>
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

(define-module (gnu packages ncdu)
  #:use-module (gnu packages)
  #:use-module (gnu packages ncurses)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public ncdu
  (package
   (name "ncdu")
   (version "1.10")
   (source (origin
	    (method url-fetch)
	    (uri (string-append "http://dev.yorhel.nl/download/ncdu-"
				version ".tar.gz"))
	    (sha256
	     (base32
	      "0rqc5wpqcbfqpcwxgh3jxwa0yw2py0hv0acpsf0a9g6v9144m6gm"))))
   (inputs
    `(("ncurses" ,ncurses)))
   (build-system gnu-build-system)
   (synopsis "Ncurses based disk usage analyzer")
   (description "A disk usage analyzer with an ncurses interface, aimed to be
run on a remote server where you don't have an entire gaphical setup, but have
to do with a simple SSH connection. ncdu aims to be fast, simple and easy to
use, and should be able to run in any minimal POSIX-like environment with
ncurses installed.")
   (license (x11-style "http://g.blicky.net/ncdu.git/plain/COPYING?id=v1.10"))
   (home-page "http://dev.yorhel.nl/ncdu")))
