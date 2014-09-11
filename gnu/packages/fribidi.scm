;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
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

(define-module (gnu packages fribidi)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public fribidi
  (package
    (name "fribidi")
    (version "0.19.6")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "http://fribidi.org/download/" name "-" version
                         ".tar.bz2"))
        (sha256
          (base32 "0zg1hpaml34ny74fif97j7ngrshlkl3wk3nja3gmlzl17i1bga6b"))))

    (build-system gnu-build-system)
    (synopsis "Implementation of the Unicode bidirectional algorithm")
    (description
     "GNU FriBidi is an implementation of the Unicode Bidirectional
Algorithm.  This algorithm is used to properly display text in left-to-right
or right-to-left ordering as necessary.")
    (home-page "http://fribidi.org/")
    (license lgpl2.1+)))
