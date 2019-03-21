;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
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

(define-module (gnu packages gpodder)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

(define-public libmygpo-qt
  (package
    (name "libmygpo-qt")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://stefan.derkits.at/files/"
                                  "libmygpo-qt/libmygpo-qt." version ".tar.gz"))
              (sha256
               (base32
                "1kg18qrq2rsswgzhl65r3mlyx7kpqg4wwnbp4yiv6svvmadmlxl2"))
              (patches (search-patches "libmygpo-qt-fix-qt-5.11.patch"
                                       "libmygpo-qt-missing-qt5-modules.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("qt" ,qtbase)))
    (arguments
     `(#:configure-flags '("-DMYGPO_BUILD_TESTS=ON")
       ;; TODO: Enable tests when https://github.com/gpodder/gpodder/issues/446
       ;; is fixed.
       #:tests? #f))
    (home-page "http://wiki.gpodder.org/wiki/Libmygpo-qt")
    (synopsis "Qt/C++ library wrapping the gpodder web service")
    (description "@code{libmygpo-qt} is a Qt/C++ library wrapping the
@url{https://gpodder.net} APIs.  It allows applications to discover, manage
and track podcasts.")
    (license license:lgpl2.1+)))
