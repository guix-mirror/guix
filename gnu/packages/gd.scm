;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix build-system perl)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:select (non-copyleft perl-license)))

(define-public gd
  (package
    (name "gd")
    (replacement gd/fixed)
    ;; Note: With libgd.org now pointing to github.com, genuine old
    ;; tarballs are no longer available.  Notably, versions 2.0.x are
    ;; missing.
    (version "2.2.5")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/libgd/libgd/releases/download/gd-"
                   version "/libgd-" version ".tar.xz"))
             (sha256
              (base32
               "0lfy5f241sbv8s3splm2zqiaxv7lxrcshh875xryryk7yk5jqc4c"))

             (patches (search-patches "gd-CVE-2018-5711.patch"
                                      "gd-CVE-2018-1000222.patch"
                                      "gd-fix-tests-on-i686.patch"
                                      "gd-freetype-test-failure.patch"))))
    (build-system gnu-build-system)
    (arguments
      ;; As recommended by github.com/libgd/libgd/issues/278 to fix rounding
      ;; issues on aarch64 and other architectures.
     `(#:make-flags '("CFLAGS=-ffp-contract=off")
       #:phases
       (modify-phases %standard-phases
         ;; This test is known to fail on i686-linux:
         ;; https://github.com/libgd/libgd/issues/359
         ;; TODO Replace this substitution with an upstream bug fix.
         (add-after 'unpack 'disable-failing-test
           (lambda _
             (substitute* "tests/gdimagegrayscale/basic.c"
               (("return gdNumFailures\\(\\)")
                 "return 0"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (license (non-copyleft "file://COPYING"
                           "See COPYING file in the distribution."))
    (properties '((cpe-name . "libgd")))))

(define-public gd/fixed
  (hidden-package
    (package
      (inherit gd)
      (source (origin
                (inherit (package-source gd))
                (patches (append (origin-patches (package-source gd))
                                 (search-patches "gd-CVE-2019-6977.patch"
                                                 "gd-CVE-2019-6978.patch"))))))))

(define-public perl-gd
  (package
    (name "perl-gd")
    (version "2.71")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RU/RURBAN/"
                           "GD-" version ".tar.gz"))
       (sha256
        (base32 "1ivskdb4nwy7ky37369hwkfxp11fkp6ri5k7qlf2dmra7f3y86s5"))))
    (build-system perl-build-system)
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gd" ,gd)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("zlib" ,zlib)))
    (arguments
     `(#:make-maker-flags
       (list (string-append "--lib_jpeg_path="
                            (assoc-ref %build-inputs "libjpeg")))))
    (home-page "https://metacpan.org/release/GD")
    (synopsis "Perl interface to the GD graphics library")
    (description "GD.pm is an autoloadable interface module for libgd, a
popular library for creating and manipulating PNG files.  With this library
you can create PNG images on the fly or modify existing files.")
    (license perl-license)))

(define-public perl-gd-securityimage
  (package
    (name "perl-gd-securityimage")
    (version "1.75")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/B/BU/BURAK/"
                           "GD-SecurityImage-" version ".tar.gz"))
       (sha256
        (base32 "19lf1kzdavrkkx3f900jnpynr55d5kjd2sdmwpfir5dsmkcj9pix"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _ (setenv "PERL_USE_UNSAFE_INC" "1") #t)))))
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-gd" ,perl-gd)
       ("perl-image-magick" ,perl-image-magick)))
    (home-page "https://metacpan.org/release/GD-SecurityImage")
    (synopsis "Security image generator")
    (description "This module provides a basic interface to create
security (captcha) images.  The final output is the actual graphic data, the
mime type of the graphic, and the created random string.  The module also has
some \"styles\" that are used to create the background (or foreground) of the
image.")
    (license perl-license)))
