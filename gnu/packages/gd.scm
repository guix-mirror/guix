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

(define-module (gnu packages gd)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) #:select (bsd-style)))

(define-public gd
  (package
    (name "gd")

    ;; Note: With libgd.org now pointing to bitbucket.org, genuine old
    ;; tarballs are no longer available.  Notably, versions 2.0.34 and .35 are
    ;; missing.
    (version "2.0.33")

    (source (origin
             (method url-fetch)
             (uri "https://bitbucket.org/libgd/gd-libgd/get/GD_2_0_33.tar.gz")
             (sha256
              (base32
               "0yrbx8mj9pykyzm0zl1q86xlkdvkajcsf5jmg688vhw9yc5wmbbw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'unpack 'chdir
                 (lambda _
                   (chdir "src"))
                 %standard-phases)))
    (inputs
     `(("freetype" ,freetype)
       ("libpng" ,libpng)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("fontconfig" ,fontconfig)
       ("libjpeg" ,libjpeg)))
    (home-page "http://www.libgd.org/")
    (synopsis "Library for the dynamic creation of images by programmers")
    (description
     "GD is a library for the dynamic creation of images by programmers.  GD
is written in C, and \"wrappers\" are available for Perl, PHP and other
languages.  GD creates PNG, JPEG, GIF, WebP, XPM, BMP images, among other
formats.  GD is commonly used to generate charts, graphics, thumbnails, and
most anything else, on the fly.  While not restricted to use on the web, the
most common applications of GD involve website development.")
    (license (bsd-style "file://COPYING"
                        "See COPYING file in the distribution."))))
