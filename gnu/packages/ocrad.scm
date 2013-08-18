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

(define-module (gnu packages ocrad)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((gnu packages compression)
                #:select (lzip)))

(define-public ocrad
  (package
    (name "ocrad")
    (version "0.22")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/ocrad/ocrad-"
                                 version ".tar.lz"))
             (sha256
              (base32
               "1zcqa4ydcmmhzyg730wywnmkjv2x2av3r875vwz69k69yjydrjfn"))))
    (build-system gnu-build-system)
    (native-inputs `(("lzip" ,lzip)))
    (home-page "http://www.gnu.org/software/ocrad/")
    (synopsis "Optical character recognition based on feature extraction")
    (description
     "GNU Ocrad is an OCR (Optical Character Recognition) program based on a
feature extraction method.  It reads images in pbm (bitmap), pgm
 (greyscale) or ppm (color) formats and produces text in byte (8-bit)
or UTF-8 formats.

Also includes a layout analyser able to separate the columns or blocks of text
normally found on printed pages.

Ocrad can be used as a stand-alone console application, or as a backend to
other programs.")
    (license gpl3+)))
