;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite))

(define-public gpsbabel
  (package
    (name "gpsbabel")
    (version "1.5.4")
    (source (origin
              (method url-fetch)
              ;; XXX: Downloads from gpsbabel.org are hidden behind a POST, so
              ;; get it from elsewhere.
              (uri (string-append
                    "mirror://debian/pool/main/g/gpsbabel/gpsbabel_"
                    version ".orig.tar.gz"))
              (sha256
               (base32
                "19hykxhyl567gf8qcrl33qhv95w0g4vxw9r3h9b8d8plx9bnaf8l"))
              (patches (search-patches
                        "gpsbabel-minizip.patch"
                        ;; XXX: Remove this patch on the next release.
                        "gpsbabel-qstring.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete files under GPL-compatible licences but never used
                  ;; on GNU systems, rather than bloating the LICENSE field.
                  (delete-file "gui/serial_mac.cc")           ; Apple MIT
                  (delete-file "mingw/include/ddk/hidsdi.h") ; public domain
                  #t))))
    (build-system gnu-build-system)
    ;; TODO: "make doc" requires Docbook & co.
    (arguments
     `(#:configure-flags
       '("--with-zlib=system"
         ;; XXX Use -fPIC to work around build problems with Qt, GCC 5, and
         ;; recent binutils:
         ;; https://codereview.qt-project.org/#/c/111787/
         "CXXFLAGS=-std=gnu++11 -fPIC")
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
    (home-page "https://www.gpsbabel.org/")
    (synopsis "Convert and exchange data with GPS and map programs")
    (description
     "GPSBabel converts waypoints, tracks, and routes between hundreds of
popular GPS receivers and mapping programs.  It contains extensive data
manipulation abilities making it a convenient for server-side processing or as
the back-end for other tools.  It does not convert, transfer, send, or
manipulate maps.")
    (license (list license:expat        ; shapelib/*.[ch]
                   license:gpl2+))))    ; everything else

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
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "006a6l8p38a4h7y2959sqrmjjn29d8pd50zj9nypcp5ph18nybjb"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; This is a rudimentary build system.
               (substitute* "Makefile"
                 (("prefix[[:blank:]]*=.*$")
                  (string-append "prefix = " (assoc-ref outputs "out")
                                 "\n")))
               #t)))
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

(define-public gama
  (package
    (name "gama")
    (version "2.03")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://gnu/gama/gama-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0d33yyasnx54c6i40rkr9by4qv92rqb8wkmp5r46nz7bbp9kpymv"))))
    (build-system gnu-build-system)
    (arguments '(#:parallel-tests? #f)) ; race condition
    (native-inputs
     `(("libxml2" ,libxml2)))
    (inputs
     `(("expat" ,expat)
       ("sqlite" ,sqlite)))
    (home-page "https://www.gnu.org/software/gama")
    (synopsis "Adjustment of geodetic networks")
    (description
     "GNU Gama is a program for the adjustment of geodetic networks.  It is
useful in measurements where Global Positioning System (GPS) is not available,
such as underground.  It features the ability to adjust in local Cartesian
coordinates as well as partial support for adjustments in global coordinate systems.")
    (license license:gpl3+)))

(define-public gpxsee
  (package
    (name "gpxsee")
    (version "4.19")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/tumic0/GPXSee/archive/"
                              version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00j0gjldw1kn3i45dppld1pz8r4s1g7lw89k7gfvvqbjjyjih1wg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; Use lrelease to convert TS translation files into QM files.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (for-each (lambda (file)
                         (invoke "lrelease" file))
                       (find-files "lang" "\\.ts"))
             (substitute* "src/config.h"
               (("/usr/share/gpxsee")
                (string-append
                 (assoc-ref outputs "out") "/share/gpxsee")))
             (invoke "qmake"
                     (string-append "PREFIX="
                                    (assoc-ref outputs "out")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/gpxsee/")))
               (install-file "GPXSee" (string-append out "/bin/GPXSee"))
               (install-file "pkg/maps.txt" share))
             #t)))))
    (inputs
     `(("qtbase" ,qtbase)))
    (native-inputs
     `(("qttools" ,qttools)))
    (home-page "http://www.gpxsee.org")
    (synopsis "GPX file viewer and analyzer")
    (description
     "GPXSee is a Qt-based GPS log file viewer and analyzer that supports GPX,
TCX, KML, FIT, IGC and NMEA files.")
    (license license:gpl3)))
