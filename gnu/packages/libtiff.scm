;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages libtiff)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages file)
  #:use-module (gnu packages libjpeg)
  #:use-module ((guix licenses) #:select (bsd-style))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libtiff
  (package
   (name "libtiff")
   (version "4.0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.remotesensing.org/pub/libtiff/tiff-"
                   version ".tar.gz"))
            (sha256 (base32
                     "0wj8d1iwk9vnpax2h29xqc2hwknxg3s0ay2d5pxkg59ihbifn6pa"))))
   (build-system gnu-build-system)
   (inputs `(("zlib" ,zlib)
             ("libjpeg-8" ,libjpeg-8)))
             ;; currently does not compile with libjpeg version 9
   (native-inputs `(("file" ,file)))
   (arguments
    `(#:configure-flags
      (list (string-append "--with-jpeg-include-dir="
                           (assoc-ref %build-inputs "libjpeg-8")
                           "/include"))
      #:phases
      (alist-cons-before
       'configure 'patch-configure
       (lambda _
         (substitute* "configure"
           (("`/usr/bin/file")
            (string-append "`" (which "file")))))
      %standard-phases)))
   (synopsis "Libtiff, a library for handling TIFF files")
   (description
    "Libtiff provides support for the Tag Image File Format (TIFF), a format
used for storing image data.
Included are a library, libtiff, for reading and writing TIFF and a small
collection of tools for doing simple manipulations of TIFF images.")
   (license (bsd-style "file://COPYRIGHT"
                       "See COPYRIGHT in the distribution."))
   (home-page "http://www.libtiff.org/")))
