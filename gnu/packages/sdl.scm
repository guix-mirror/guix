;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 David Thompson <dthompson2@worcester.edu>
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

(define-module (gnu packages sdl)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:export (libmikmod))

(define libmikmod
  (package
    (name "libmikmod")
    (version "3.3.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/mikmod/libmikmod/"
                                 version "/libmikmod-" version ".tar.gz"))
             (sha256
              (base32
               "0dr4kgvhq9wf2riibh178c2al996spwwak6zffpv5n5bqmw29w3r"))))
    (build-system gnu-build-system)
    (inputs `(("alsa-lib" ,alsa-lib)
              ("libx11" ,libx11)))
    (synopsis "Library for module sound formats.")
    (description
     "MikMod is able to play a wide range of module formats, as well as
digital sound files. It can take advantage of particular features of your
system, such as sound redirection over the network.")
    (license lgpl2.1)
    (home-page "http://mikmod.sourceforge.net/")))
