;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages gps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages geeqie)              ;exiv2
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages qt))

(define-public gpsbabel
  (package
    (name "gpsbabel")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              ;; XXX: Downloads from gpsbabel.org are hidden behind a POST, so
              ;; get it from elsewhere.
              (uri (string-append
                    "mirror://debian/pool/main/g/gpsbabel/gpsbabel_"
                    version ".orig.tar.gz"))
              (sha256
               (base32
                "0xf7wmy2m29g2lm8lqc74yf8rf7sxfl3cfwbk7dpf0yf42pb0b6w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--with-zlib=system"
         "CXXFLAGS=-std=gnu++11")
       #:phases
       (modify-phases %standard-phases
        (add-before 'configure 'pre-configure
                    (lambda _
                      (chdir "gpsbabel"))))
                    ;; TODO: "make doc" requires Docbook & co.

       ;; On i686, 'raymarine.test' fails because of a rounding error:
       ;; <http://hydra.gnu.org/build/133040>.  As a workaround, disable tests
       ;; on these platforms.
       ;; FIXME: On x86_64 with -std=gnu++11 tests also fail due to rounding
       ;; error.
       #:tests? #f))
    (inputs
     `(("expat" ,expat)
       ("zlib" ,zlib)
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)))
    (native-inputs
     `(("which" ,which)
       ("libxml2" ,libxml2)))              ;'xmllint' needed for the KML tests
    (home-page "http://www.gpsbabel.org/")
    (synopsis "Convert and exchange data with GPS and map programs")
    (description
     "GPSBabel converts waypoints, tracks, and routes between hundreds of
popular GPS receivers and mapping programs.  It contains extensive data
manipulation abilities making it a convenient for server-side processing or as
the back-end for other tools.  It does not convert, transfer, send, or
manipulate maps.")
    (license license:gpl2+)))

(define-public gpscorrelate
  ;; This program is "lightly maintained", so to speak, so we end up taking it
  ;; directly from its Git repo.
  (let ((commit "365f6e1b3f"))
    (package
      (name "gpscorrelate")
      (version (string-append "1.6.1." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/freefoote/gpscorrelate")
                      (commit commit)))
                (sha256
                 (base32
                  "006a6l8p38a4h7y2959sqrmjjn29d8pd50zj9nypcp5ph18nybjb"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (alist-replace
                   'configure
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     ;; This is a rudimentary build system.
                     (substitute* "Makefile"
                       (("prefix[[:blank:]]*=.*$")
                        (string-append "prefix = " (assoc-ref outputs "out")
                                       "\n"))))
                   %standard-phases)
         #:tests? #f))
      (inputs
       `(("gtk+" ,gtk+-2)
         ("libxml2" ,libxml2)
         ("exiv2" ,exiv2)))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("docbook-xml" ,docbook-xml)
         ("docbook-xsl" ,docbook-xsl)
         ("libxslt" ,libxslt)))
      (home-page "http://freefoote.dview.net/linux/gpscorrelate")
      (synopsis "GPS photo correlation tool to geo-localize images")
      (description
       "GPS Correlate is a program to match a recorded GPS track with the EXIF
tags in digital camera photos, and update the EXIF tags with the location that
the photo was taken.  It does this by using the timestamp in the photo and
finding a data point in the GPS track that matches, or interpolating a point
between two other data points.")
      (license license:gpl2+))))
