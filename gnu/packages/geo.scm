;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017, 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018, 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019, 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages geo)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system r)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages java)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml))

(define-public geos
  (package
    (name "geos")
    (version "3.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.osgeo.org/geos/geos-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1mb2v9fy1gnbjhcgv0xny11ggfb17vkzsajdyibigwsxr4ylq4cr"))))
    (build-system gnu-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (add-after
                    'unpack 'patch-test-shebangs
                    (lambda _
                      (substitute* '("tests/xmltester/testrunner.sh"
                                     "tests/geostest/testrunner.sh")
                        (("/bin/sh") (which "sh")))
                      #t)))))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://geos.osgeo.org/")
    (synopsis "Geometry Engine for Geographic Information Systems")
    (description
     "GEOS provides a spatial object model and fundamental geometric
functions.  It is a C++ port of the Java Topology Suite (JTS).  As such,
it aims to contain the complete functionality of JTS in C++.  This
includes all the OpenGIS Simple Features for SQL spatial predicate
functions and spatial operators, as well as specific JTS enhanced
topology functions.")
    (license (list license:lgpl2.1+          ; Main distribution.
                   license:zlib              ; tests/xmltester/tinyxml/*
                   license:public-domain)))) ; include/geos/timeval.h

(define-public gnome-maps
  (package
    (name "gnome-maps")
    (version "3.32.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1m191iq1gjaqz79ci3dkbmwrkxp7pzknngimlf5bqib5x8yairlb"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                   (geocode-glib-path (string-append
                                       (assoc-ref inputs "geocode-glib")
                                       "/lib"))
                   (goa-path (string-append
                              (assoc-ref inputs "gnome-online-accounts:lib")
                              "/lib"))
                   (gdk-pixbuf-path (string-append
                                     (assoc-ref inputs "gdk-pixbuf")
                                     "/lib"))
                   (webkitgtk-path (string-append
                                    (assoc-ref inputs "webkitgtk")
                                    "/lib")))
               (wrap-program (string-append out "/bin/gnome-maps")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))

                 ;; There seems to be no way to embed the path of
                 ;; libgoa-1.0.so.0, libwebkit2gtk-4.0.so.37,
                 ;; libgdk_pixbuf-2.0.so, libjavascriptcoregtk-4.0.so.18, and
                 ;; libgeocode-glib.so.0
                 `("LD_LIBRARY_PATH" ":" prefix (,goa-path
                                                 ,webkitgtk-path
                                                 ,gdk-pixbuf-path
                                                 ,geocode-glib-path)))
               #t))))))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("folks" ,folks)
       ("libchamplain" ,libchamplain)
       ("libgee" ,libgee)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("libgweather" ,libgweather)
       ("libxml2" ,libxml2)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib-networking" ,glib-networking)
       ("geoclue" ,geoclue)
       ("geocode-glib" ,geocode-glib)
       ("gfbgraph" ,gfbgraph)
       ("gjs" ,gjs)
       ("glib" ,glib)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("rest" ,rest)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(("gtk+3" ,gtk+)))
    (synopsis "Graphical map viewer and wayfinding program")
    (description "GNOME Maps is a graphical map viewer.  It uses map data from
the OpenStreetMap project.  It can provide directions for walking, bicycling,
and driving.")
    (home-page "https://wiki.gnome.org/Apps/Maps")
    (license license:gpl2+)))

(define-public libgaiagraphics
  (package
    (name "libgaiagraphics")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gaia-gis.it/gaia-sins/libgaiagraphics-"
                           version ".tar.gz"))
       (sha256
        (base32
         "076afqv417ag3hfvnif0qc7qscmnq1dsf6y431yygwgf34rjkayc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("libpng" ,libpng)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("libgeotiff" ,libgeotiff)
       ("proj.4" ,proj.4)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
     (synopsis "Gaia common graphics support")
     (description "libgaiagraphics is a library supporting
 common-utility raster handling methods.")
    (home-page "https://www.gaia-gis.it/fossil/libgaiagraphics/index")
    (license license:lgpl3+)))

(define-public libgeotiff
  (package
    (name "libgeotiff")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.osgeo.org/geotiff/libgeotiff/libgeotiff-"
                           version ".tar.gz"))
       (patches (search-patches
                 ;; See libgeotiff 1.5.1 issue
                 ;; https://github.com/OSGeo/libgeotiff/issues/22
                 "libgeotiff-adapt-test-script-for-proj-6.2.patch"))
       (sha256
        (base32 "0b31mlzcv5b1y7jdvb7p0pa3xradrg3x5g32ym911lbhq4rrgsgr"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove .csv files, distributed from EPSG under a restricted
           ;; license. See LICENSE for full license text.
           (for-each delete-file (find-files "." "\\.csv$"))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("libjpeg-turbo" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(;; libgeotiff headers include proj headers, so ensure those are available.
       ("proj" ,proj)))
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             "--with-zlib" "--with-jpeg"
             (string-append "--with-libtiff=" (assoc-ref %build-inputs "libtiff")))))
    (synopsis "Library for handling GeoTIFF (geographic enabled TIFF)")
    (description "libgeotiff is a library on top of libtiff for reading and
writing GeoTIFF information tags.")
    (home-page "https://trac.osgeo.org/geotiff/")
    ;; This is a mixture of various contributions under different licenses.
    ;; Note that the EPSG database is NOT "free to use" as the LICENSE file
    ;; states, as its commercial redistribution is restricted. Hence, we have
    ;; removed it from the package.
    (license (list license:public-domain
                   license:x11
                   license:bsd-3
                   (license:non-copyleft "file://LICENSE"
                                         "See LICENSE in the distribution.")))))

(define-public libspatialite
  (package
    (name "libspatialite")
    (version "4.3.0a")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gaia-gis.it/gaia-sins/libspatialite-"
                           version ".tar.gz"))
       (sha256
        (base32
         "16d4lpl7xrm9zy4gphy6nwanpjp8wn9g4wq2i2kh8abnlhq01448"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freexl" ,freexl)
       ("geos" ,geos)
       ("libxml2" ,libxml2)
       ("proj.4" ,proj.4)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; 3 tests are failing, ignore them:
         (add-after 'unpack 'ignore-broken-tests
           (lambda _
             (substitute* '("test/Makefile.in")
               (("\tcheck_sql_stm.*" all) "\tcheck_multithread$(EXEEXT) \\\n")
               (("(\tch.*) check_v.*ble2.*$" all vt1) (string-append vt1 " \\\n"))
               (("\tch.* (check_v.*ble4.*)$" all vt4) (string-append "\t" vt4)))
             #t)))))
    (synopsis "Extend SQLite to support Spatial SQL capabilities")
    (description
     "SpatiaLite is a library intended to extend the SQLite core to support
fully fledged Spatial SQL capabilities.")
    (home-page "https://www.gaia-gis.it/fossil/libspatialite/index")
    ;; For the genuine libspatialite-sources holds:
    ;; Any of the licenses MPL1.1, GPL2+ or LGPL2.1+  may be picked.
    ;; Files under src/control_points are from GRASS
    ;; and are licensed under GPL2+ only.
    ;; src/md5.[ch]: Placed into the public domain by Alexander Peslyak.
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1
                   license:public-domain))))

(define-public proj
  (package
    (name "proj")
    (version "6.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.osgeo.org/proj/proj-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1y46ij32j9b4x1kjnnlykcwk3kkjwkg44sfc1ziwm3a3g0ki3q3d"))))
    (build-system gnu-build-system)
    (inputs
     `(("sqlite" ,sqlite)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://proj.org/")
    (synopsis "Coordinate transformation software")
    (description
     "Proj is a generic coordinate transformation software that transforms
geospatial coordinates from one coordinate reference system (CRS) to another.
This includes cartographic projections as well as geodetic transformations.
PROJ includes command line applications for easy conversion of coordinates
from text files or directly from user input.  In addition, proj also exposes
an application programming interface that lets developers use the
functionality of proj in their own software.")
    (license (list license:expat
                   ;; src/projections/patterson.cpp
                   license:asl2.0
                   ;; src/geodesic.*, src/tests/geodtest.cpp
                   license:x11))))

(define-public proj.4
  (package
    (name "proj.4")
    (version "4.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.osgeo.org/proj/proj-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xw5f427xk9p2nbsj04j6m5zyjlyd66sbvl2bkg8hd1kx8pm9139"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-test-paths
           (lambda _
             (substitute* '("nad/test27"
                            "nad/test83"
                            "nad/testvarious"
                            "nad/testdatumfile"
                            "nad/testflaky"
                            "nad/testIGNF")
               (("/bin/rm") (which "rm")))
             #t))
         ;; Precision problems on i686 and other platforms. See:
         ;; https://web.archive.org/web/20151006134301/http://trac.osgeo.org/proj/ticket/255
         ;; Disable failing test.
         (add-after 'patch-test-paths 'ignore-failing-tests
           (lambda _
             (substitute* '("nad/Makefile.in")
               (("\tPROJ_LIB.*" all) (string-append  "#" all)))
             #t)))))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://proj.org/")
    (synopsis "Cartographic Projections Library")
    (description
     "Proj.4 is a library for converting coordinates between cartographic
projections.")
    (license (list license:expat
                   ;; src/PJ_patterson.c
                   license:asl2.0
                   ;; src/geodesic.c/h
                   license:x11
                   ;; Embedded EPSG database.
                   (license:non-copyleft "http://www.epsg.org/TermsOfUse")
                   ;; cmake/*
                   license:boost1.0))))

(define-public mapnik
  (package
    (name "mapnik")
    (version "3.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mapnik/mapnik/releases/download/v"
                           version "/mapnik-v" version ".tar.bz2"))
       (sha256
        (base32
         "06frcikaj2mgz3abfk5h0z4j3hbksi0zikwjngbjv4p5f3pwxf8q"))))
    (build-system scons-build-system)
    (inputs
     `(("boost" ,boost)
       ("cairo" ,cairo)
       ("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("libxml2" ,libxml2)
       ("proj.4" ,proj.4)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags
       (list "CC=gcc"
             (string-append "PREFIX=" %output)
             (string-append "CUSTOM_LDFLAGS=-Wl,-rpath=" %output "/lib"))))
    (home-page "https://mapnik.org/")
    (synopsis "Toolkit for developing mapping applications")
    (description "Mapnik is a toolkit for developing mapping applications.  It
is basically a collection of geographic objects like maps, layers,
datasources, features, and geometries.  At its core is a C++ shared library
providing algorithms and patterns for spatial data access and visualization.
The library does not rely on any specific windowing system and can be deployed
to any server environment.  It is intended to play fair in a multi-threaded
environment and is aimed primarily, but not exclusively, at web-based
development.")
    (license (list license:lgpl2.1+
                   ;; demo/viewer, demo/python/rundemo.py
                   license:gpl2+
                   ;; deps/boost, deps/mapbox, deps/agg/include/agg_conv_offset.h
                   license:boost1.0
                   ;; deps/mapnik/sparsehash
                   license:bsd-3
                   ;; deps/agg
                   (license:non-copyleft "file://deps/agg/copying")))))

(define-public python2-mapnik
  (package
    (name "python2-mapnik")
    (version "3.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mapnik/python-mapnik/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0w7wg72gnwmbjani9sqk42p2jwqkrl9hsdkawahni5m05xsifcb4"))))
    (build-system python-build-system)
    (inputs
     `(("boost" ,boost)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("mapnik" ,mapnik)
       ("proj.4" ,proj.4)
       ("python2-pycairo" ,python2-pycairo)))
    (native-inputs
     (let ((test-data-input
            (lambda (repository version hash)
              (origin
                (method url-fetch)
                (uri (string-append "https://github.com/mapnik/" repository
                                    "/archive/v" version ".tar.gz"))
                (file-name (string-append "python-mapnik-" repository
                                          "-" version ".tar.gz"))
                (sha256 (base32 hash))))))
       `(("python2-nose" ,python2-nose)
         ;; Test data is released as separate tarballs
         ("test-data"
          ,(test-data-input "test-data" "3.0.18"
                            "10cvgn5gxn8ldrszj24zr1vzm5w76kqk4s7bl2zzp5yvkhh8lj1n"))
         ("test-data-visual"
          ,(test-data-input "test-data-visual" "3.0.18"
                            "1cb9ghy8sis0w5fkp0dvwxdqqx44rhs9a9w8g9r9i7md1c40r80i")))))
    (arguments
     `(#:python ,python-2 ; Python 3 support is incomplete, and the build fails
       #:phases
       (modify-phases %standard-phases
         ;; Unpack test data into the source tree
         (add-after 'unpack 'unpack-submodules
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((unpack (lambda (source target)
                             (with-directory-excursion target
                               (invoke "tar" "xvf" (assoc-ref inputs source)
                                       "--strip-components=1")))))
               (unpack "test-data" "test/data")
               (unpack "test-data-visual" "test/data-visual"))))
         ;; Skip failing tests
         (add-after 'unpack 'skip-tests
           (lambda _
             (let ((skipped-tests (list "test_vrt_referring_to_missing_files"
                                        "test_unicode_regex_replace"
                                        "test_proj_antimeridian_bbox"
                                        "test_render_with_scale_factor")))
               (substitute* "setup.cfg"
                 (("\\[nosetests\\]" all)
                  (string-append all "\nexclude=^("
                                 (string-join skipped-tests "|") ")$")))))))))
    (home-page "https://github.com/mapnik/python-mapnik")
    (synopsis "Python bindings for Mapnik")
    (description "This package provides Python bindings for Mapnik.")
    (license license:lgpl2.1+)))

(define-public spatialite-gui
  (package
    (name "spatialite-gui")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gaia-gis.it/gaia-sins/spatialite_gui-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1r05dz9pyc8vsd2wbqxcsracpfbaamz470rcyp2myfpqwznv376b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freexl" ,freexl)
       ("geos" ,geos)
       ("libgaiagraphics" ,libgaiagraphics)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libspatialite" ,libspatialite)
       ("libxml2" ,libxml2)
       ("proj.4" ,proj.4)
       ("sqlite" ,sqlite)
       ("wxwidgets" ,wxwidgets-2)
       ("zlib" ,zlib)))
    (synopsis "Graphical user interface for SpatiaLite")
    (description "Spatialite-gui provides a visual interface for viewing and
 maintaining a spatialite database.  You can easily see the structure of the
 tables and data contents using point and click functions, many of which
 construct common SQL queries, or craft your own SQL queries.")
    (home-page "https://www.gaia-gis.it/fossil/spatialite_gui/index")
    (license license:gpl3+)))

(define-public gdal
  (package
    (name "gdal")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "http://download.osgeo.org/gdal/" version "/gdal-"
                     version ".tar.gz"))
              (sha256
               (base32
                "10symyajj1b7j98f889lqxxbmhcyvlhq9gg0l42h69bv22wx45gw"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   ;; TODO: frmts contains a lot more bundled code.
                   (for-each delete-file-recursively
                     ;; bundled code
                     '("frmts/png/libpng"
                       "frmts/gif/giflib"
                       "frmts/jpeg/libjpeg"
                       "frmts/jpeg/libjpeg12"
                       "frmts/gtiff/libtiff"
                       "frmts/gtiff/libgeotiff"
                       "frmts/zlib"
                       "ogr/ogrsf_frmts/geojson/libjson"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (let-syntax ((with (syntax-rules ()
                            ((_ option input)
                             (string-append option "="
                                            (assoc-ref %build-inputs input))))))
         (list
           ;; TODO: --with-pcidsk, --with-pcraster
           (with "--with-freexl" "freexl")
           (with "--with-libjson-c" "json-c")
           (with "--with-png" "libpng")
           (with "--with-webp" "libwebp")
           (with "--with-gif" "giflib")
           (with "--with-jpeg" "libjpeg-turbo")
           (with "--with-libtiff" "libtiff")
           (with "--with-geotiff" "libgeotiff")
           (with "--with-libz" "zlib")
           (with "--with-expat" "expat")
           (with "--with-sqlite3" "sqlite")
           "--with-pcre"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-path
           (lambda _
             (substitute* "frmts/mrf/mrf_band.cpp"
               (("\"../zlib/zlib.h\"") "<zlib.h>")))))))
    (inputs
     `(("expat" ,expat)
       ("freexl" ,freexl)
       ("geos" ,geos)
       ("giflib" ,giflib)
       ("json-c" ,json-c)
       ("libgeotiff" ,libgeotiff)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("netcdf" ,netcdf)
       ("pcre" ,pcre)
       ("proj" ,proj)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (home-page "https://gdal.org/")
    (synopsis "Raster and vector geospatial data format library")
    (description "GDAL is a translator library for raster and vector geospatial
data formats.  As a library, it presents a single raster abstract data model
and single vector abstract data model to the calling application for all
supported formats.  It also comes with a variety of useful command line
utilities for data translation and processing.")
    (license (list
               ;; general license
               license:expat
               ;; frmts/gtiff/tif_float.c, frmts/pcraster/libcsf,
               ;; ogr/ogrsf_frmts/dxf/intronurbs.cpp, frmts/pdf/pdfdataset.cpp
               ;; frmts/mrf/
               license:bsd-3
               ;; frmts/hdf4/hdf-eos/*
               ;; similar to the expat license, but without guarantee exclusion
               (license:non-copyleft "file://frmts/hdf4/hdf-eos/README")
               ;; frmts/grib/degrib/
               license:public-domain ; with guarantee exclusion
               ;; port/cpl_minizip*
               ;; Some bsd-inspired license
               (license:non-copyleft "file://port/LICENCE_minizip")
               ;; alg/internal_libqhull
               ;; Some 5-clause license
               (license:non-copyleft "file://alg/internal_libqhull/COPYING.txt")
               ;; frmts/mrf/libLERC
               license:asl2.0))))

(define-public python-gdal
  (package (inherit gdal)
    (name "python-gdal")
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir
           (lambda _
             (chdir "swig/python")
             #t)))))
    (native-inputs '())
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (inputs
     `(("gdal" ,gdal)))
    (synopsis "GDAL (Geospatial Data Abstraction Library) python bindings")))

(define-public postgis
  (package
    (name "postgis")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.osgeo.org/postgis/source/postgis-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15557fbk0xkngihwhqsbdyz2ng49blisf5zydw81j0gabk6x4vy0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "datadir=" (assoc-ref %outputs "out") "/share")
             (string-append "docdir="(assoc-ref %outputs "out") "/share/doc")
             (string-append "pkglibdir="(assoc-ref %outputs "out") "/lib")
             (string-append "bindir=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("raster/loader/Makefile" "raster/scripts/python/Makefile")
               (("\\$\\(DESTDIR\\)\\$\\(PGSQL_BINDIR\\)")
                (string-append (assoc-ref outputs "out") "/bin"))))))))
    (inputs
     `(("gdal" ,gdal)
       ("geos" ,geos)
       ("json-c" ,json-c)
       ("libjpeg" ,libjpeg-turbo)
       ("libxml2" ,libxml2)
       ("pcre" ,pcre)
       ("postgresql" ,postgresql)
       ("proj" ,proj)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "https://postgis.net")
    (synopsis "Spatial database extender for PostgreSQL")
    (description "PostGIS is a spatial database extender for PostgreSQL
object-relational database.  It adds support for geographic objects allowing
location queries to be run in SQL.  This package provides a PostgreSQL
extension.")
    (license (list
               ;; General license
               license:gpl2+
               ;; loader/dbfopen, safileio.*, shapefil.h, shpopen.c
               license:expat
               ;; loader/getopt.*
               license:public-domain
               ;; doc/xsl
               license:bsd-3 ; files only say "BSD"
               ;; doc
               license:cc-by-sa3.0))))

(define-public tegola
  (package
    (name "tegola")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/go-spatial/tegola/archive/v"
                     version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09vnzxfn0r70kmd776kcdfqxhzdj11syxa0b27z4ci1k367v7viw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-spatial/tegola/cmd/tegola"
       #:unpack-path "github.com/go-spatial/tegola"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-version
           (lambda _
             (with-directory-excursion "src/github.com/go-spatial/tegola"
               (substitute* '("cmd/tegola/cmd/root.go"
                              "cmd/tegola_lambda/main.go")
                 (("version not set") ,version)))
             #t)))))
    (home-page "http://tegola.io")
    (synopsis "Vector tile server for maps")
    (description "Tegola is a free vector tile server written in Go.  Tegola
takes geospatial data and slices it into vector tiles that can be efficiently
delivered to any client.")
    (license (list
               license:expat
               ;; Some packages in vendor have other licenses
               license:asl2.0
               license:bsd-2
               license:bsd-3
               license:wtfpl2))))

(define-public imposm3
  (package
    (name "imposm3")
    (version "0.6.0-alpha.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/omniscale/imposm3/archive/v"
                            version ".tar.gz"))
    (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06f0kwmv52yd5m9jlckqxqmkf0cnqy3hamakrvg9lspplyqrds80"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/omniscale/imposm3/cmd/imposm"
       #:unpack-path "github.com/omniscale/imposm3"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-version
           (lambda _
             (substitute* "src/github.com/omniscale/imposm3/version.go"
               (("0.0.0-dev") ,version))
             #t)))))
    (inputs
     `(("geos" ,geos)
       ("leveldb" ,leveldb)))
    (home-page "https://imposm.org/")
    (synopsis "OpenStreetMap importer for PostGIS")
    (description "Imposm is an importer for OpenStreetMap data.  It reads PBF
files and imports the data into PostgreSQL/PostGIS databases.  It is designed
to create databases that are optimized for rendering/tile/map-services.")
    (license (list
               license:asl2.0
               ;; Some dependencies in vendor have different licenses
               license:expat
               license:bsd-2
               license:bsd-3))))

(define-public libosmium
  (package
    (name "libosmium")
    (version "2.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/osmcode/libosmium.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "123ri1l0a2b9fljgpwsl7z2w4i3kmgxz79d4ns9z4mwbp8sw0250"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("boost" ,boost)
       ("expat" ,expat)
       ("gdal" ,gdal)
       ("geos" ,geos)
       ("proj.4" ,proj.4)
       ("protozero" ,protozero)
       ("sparsehash" ,sparsehash)
       ("zlib" ,zlib)))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "https://osmcode.org/libosmium/")
    (synopsis "C++ library for working with OpenStreetMap data")
    (description "Libosmium is a fast and flexible C++ library for working with
OpenStreetMap data.")
    (license license:boost1.0)))

(define-public osm2pgsql
  (package
    (name "osm2pgsql")
    (version "0.96.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openstreetmap/osm2pgsql.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "032cydh8ynaqfhdzmkvgbmqyjql668y6qln1l59l2s3ni9963bbl"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "contrib/protozero")
           (delete-file-recursively "contrib/libosmium")
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f; tests fail because we need to setup a database
       #:configure-flags
       (list (string-append "-DOSMIUM_INCLUDE_DIR="
                            (assoc-ref %build-inputs "libosmium")
                            "/include")
             (string-append "-DPROTOZERO_INCLUDE_DIR="
                            (assoc-ref %build-inputs "protozero")
                            "/include"))))
    (inputs
     `(("boost" ,boost)
       ("expat" ,expat)
       ("libosmium" ,libosmium)
       ("lua" ,lua)
       ("postgresql" ,postgresql)
       ("proj.4" ,proj.4)
       ("protozero" ,protozero)
       ("zlib" ,zlib)))
    (native-inputs
     `(("python-2" ,python-2)
       ("python2-psycopg2" ,python2-psycopg2)))
    (home-page "https://github.com/openstreetmap/osm2pgsql")
    (synopsis "OSM data importer to postgresql")
    (description "Osm2pgsql is a tool for loading OpenStreetMap data into a
PostgreSQL / PostGIS database suitable for applications like rendering into a
map, geocoding with Nominatim, or general analysis.")
    (license license:gpl2+)))

(define-public tippecanoe
  (package
    (name "tippecanoe")
    (version "1.31.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mapbox/tippecanoe.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m0x931a945sr7axyhcvpwh798m58hx1zxh6ikgf9gsgqhdhmszz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases (delete 'configure))
       #:test-target "test"
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (inputs
     `(("perl" ,perl)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (home-page "https://github.com/mapbox/tippecanoe")
    (synopsis "Vector tile server for maps")
    (description "Tippecanoe creates scale-independent view of data, so that
the texture and density of features is visible at every zoom level, instead of
dropping features at lower levels.")
    (license license:bsd-2)))

(define-public osmctools
  (package
    (name "osmctools")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/osm-c-tools/osmctools")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m8d3r1q1v05pkr8k9czrmb4xjszw6hvgsf3kn9pf0v14gpn4r8f"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://gitlab.com/osm-c-tools/osmctools")
    (synopsis "Tools to convert, filter and update OpenStreetMap data files")
    (description "This project contains a few tools which are used in the
OpenStreetMap project.  They can be used to convert, filter and update
OpenStreetMap data files.")
    (license license:agpl3)))

(define-public osm-gps-map
  (package
    (name "osm-gps-map")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nzjrs/osm-gps-map/releases/download/"
             version "/osm-gps-map-" version ".tar.gz"))
       (sha256
        (base32
         "11imsf4cz1dpxdjh178k2s29axmq86rkfg1pqmn7incyxmjzhbwg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gnome-common" ,gnome-common)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("libsoup" ,libsoup)))
    (home-page "https://nzjrs.github.io/osm-gps-map/")
    (synopsis "GTK+ widget for displaying OpenStreetMap tiles")
    (description
     "This package provides a GTK+ widget (and Python bindings) that when
given GPS coordinates,draws a GPS track, and points of interest on a moving
map display.  Downloads map data from a number of websites, including
@url{https://www.openstreetmap.org}.")
    (license license:gpl2+)))

(define-public xygrib
  (package
    (name "xygrib")
    (version "1.2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/opengribs/XyGrib.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xzsm8pr0zjk3f8j880fg5n82jyxn8xf1330qmmq1fqv7rsrg9ia"))
              (modules '((guix build utils)))
              (snippet
               '(begin (delete-file-recursively "data/fonts") #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-directories
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jpeg (assoc-ref inputs "openjpeg"))
                   (font (assoc-ref inputs "font-liberation")))
               (substitute* "CMakeLists.txt"
                 ;; Find libjpeg.
                 (("/usr") jpeg)
                 ;; Fix install locations.
                 (("set\\(PREFIX_BIN.*") "set(PREFIX_BIN \"bin\")\n")
                 (("set\\(PREFIX_PKGDATA.*") "set(PREFIX_PKGDATA \"share/${PROJECT_NAME}\")\n")
                 ;; Skip looking for the static library.
                 (("\"libnova.a\"") ""))
               ;; Don't use the bundled font-liberation.
               (substitute* "src/util/Font.cpp"
                 (("Util::pathFonts\\(\\)\\+\"liberation-fonts/\"")
                  (string-append "\"" font "/share/fonts/truetype/\"")))
               (substitute* "src/util/Util.h"
                 (("pathData\\(\\)\\+\"data/fonts/\"")
                  (string-append "\"" font "/share/fonts/\""))))
             #t)))
       #:tests? #f)) ; no tests
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("bzip2" ,bzip2)
       ("font-liberation" ,font-liberation)
       ("libnova" ,libnova)
       ("libpng" ,libpng)
       ("openjpeg" ,openjpeg)
       ("proj.4" ,proj.4)
       ("qtbase" ,qtbase)
       ("zlib" ,zlib)))
    (synopsis "Weather Forecast Visualization")
    (description
     "XyGrib is a Grib file reader and visualizes meteorological data providing
an off-line capability to analyse weather forecasts or hindcasts.  It is
intended to be used as a capable weather work station for anyone with a serious
interest in examining weather. This would include members of the sailing
community, private and sport aviators, farmers, weather buffs and many more.
XyGrib is the continuation of the zyGrib software package with a new team of
volunteers.")
    (home-page "https://opengribs.org")
    (license license:gpl3+)))

(define-public libspatialindex
  (package
    (name "libspatialindex")
    (version "1.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libspatialindex/libspatialindex/"
                           "releases/download/" version "/spatialindex-src-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "02n5vjcyk04w0djidyp21hfbxfpbbara8ifd9nml6158rwqr8lja"))))
    (build-system cmake-build-system)
    (home-page "https://libspatialindex.org")
    (synopsis "Spatial indexing library")
    (description "The purpose of this library is to provide:

@itemize
 @item An extensible framework that will support robust spatial indexing
methods.
 @item Support for sophisticated spatial queries.  Range, point location,
 nearest neighbor and k-nearest neighbor as well as parametric queries (defined
by spatial constraints) should be easy to deploy and run.
 @item Easy to use interfaces for inserting, deleting and updating information.
 @item Wide variety of customization capabilities.  Basic index and storage
characteristics like the page size, node capacity, minimum fan-out, splitting
algorithm, etc. should be easy to customize.
 @item Index persistence.  Internal memory and external memory structures
should be supported.  Clustered and non-clustered indices should be easy to be
persisted.
@end itemize
")
    (license license:expat)))

(define-public java-jmapviewer
  (package
    (name "java-jmapviewer")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://svn.openstreetmap.org/applications/"
                                  "viewer/jmapviewer/releases/" version
                                  "/JMapViewer-" version "-Source.zip"))
              (sha256
               (base32
                "0sy6r5fkbb9bclw0is6gwnbzz627m7pjfnsqydxz58pbndakkhrv"))))
    (build-system ant-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:build-target "pack"
       #:tests? #f; No tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'clean
           (lambda* _
             (invoke "ant" "clean")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/share/java/")))
               (mkdir-p dir)
               (copy-file "JMapViewer.jar" (string-append dir "JMapViewer.jar"))))))))
    (home-page "https://wiki.openstreetmap.org/wiki/JMapViewer")
    (synopsis "OSM map integration in Java")
    (description "JMapViewer is a Java component which allows to easily
integrate an OSM map view into your Java application.  It is maintained as
an independent project by the JOSM team.")
    (license license:gpl2)))

(define-public josm
  (package
    (name "josm")
    (version "15937")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                     (url "https://josm.openstreetmap.de/svn/trunk")
                     (revision (string->number version))
                     (recursive? #f)))
              (sha256
               (base32
                "00b8sw0wgkcf7xknmdpn5s521ax8x2660figidcrry37sgq3x946"))
              (file-name (string-append name "-" version "-checkout"))
              (modules '((guix build utils)))
            (snippet
             '(begin
		(for-each delete-file (find-files "." ".*.jar$"))
                #t))))
    (build-system ant-build-system)
    (native-inputs
     `(("javacc" ,javacc)))
    (inputs
     `(("java-commons-jcs" ,java-commons-jcs)
       ("java-commons-compress" ,java-commons-compress)
       ("java-jmapviewer" ,java-jmapviewer)
       ("java-jsonp-api" ,java-jsonp-api)
       ("java-jsonp-impl" ,java-jsonp-impl); runtime dependency
       ("java-metadata-extractor" ,java-metadata-extractor)
       ("java-openjfx-media" ,java-openjfx-media)
       ("java-signpost-core" ,java-signpost-core)
       ("java-svg-salamander" ,java-svg-salamander)))
    (arguments
     `(#:tests? #f
       #:jar-name "josm.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'rm-build.xml
           (lambda* _
             (delete-file "build.xml")
             #t))
         (add-before 'build 'fix-revision
           (lambda* _
             (with-output-to-file "REVISION.XML"
               (lambda _
                 (display
                   (string-append "<info><entry><commit revision=\"" ,version "\">"
                                  "<date>1970-01-01 00:00:00 +0000</date>"
                                  "</commit></entry></info>"))))
             #t))
         (add-before 'build 'fix-classpath
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CLASSPATH"
                     (string-join
                       (filter
                         (lambda (s)
                           (let ((source (assoc-ref inputs "source")))
                             (not (equal? (substring s 0 (string-length source)) source))))
                         (string-split (getenv "CLASSPATH") #\:))
                       ":"))
             #t))
         (add-before 'build 'generate-parser
           (lambda* _
             (let* ((dir "src/org/openstreetmap/josm/gui/mappaint/mapcss")
                    (out (string-append dir "/parsergen"))
                    (file (string-append dir "/MapCSSParser.jj")))
               (mkdir-p "src/org/openstreetmap/josm/gui/mappaint/mapcss/parsergen")
               (invoke "javacc" "-DEBUG_PARSER=false"
                       "-DEBUG_TOKEN_MANAGER=false" "-JDK_VERSION=1.8"
                       "-GRAMMAR_ENCODING=UTF-8"
                       (string-append "-OUTPUT_DIRECTORY=" out)
                       file))
             #t))
         (add-after 'build 'generate-epsg
           (lambda _
             (system* "javac" "scripts/BuildProjectionDefinitions.java"
                      "-cp" "build/classes")
             (mkdir-p "data/projection")
             (with-output-to-file "data/projection/custom-epsg"
               (lambda _ (display "")))
             (invoke "java" "-cp" "build/classes:scripts:."
                     "BuildProjectionDefinitions" ".")
             #t))
         (add-after 'generate-epsg 'copy-data
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "data" "build/classes/data")
             #t))
         (add-before 'install 'regenerate-jar
           (lambda _
             ;; We need to regenerate the jar file to add data.
             (delete-file "build/jar/josm.jar")
             (invoke "jar" "-cf" "build/jar/josm.jar" "-C"
                     "build/classes" ".")
             #t))
         (add-before 'build 'copy-styles
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "styles" "build/classes/styles")
             #t))
         (add-before 'build 'copy-images
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "images" "build/classes/images")
             #t))
         (add-before 'build 'copy-revision
           (lambda _
             (mkdir-p "build/classes")
             (with-output-to-file "build/classes/REVISION"
               (lambda _
                 (display
                   (string-append "Revision: " ,version "\n"
                                  "Is-Local-Build: true\n"
                                  "Build-Date: 1970-01-01 00:00:00 +0000\n"))))
             #t))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/josm")
                 (lambda _
                   (display
                     (string-append "#!/bin/sh\n"
                                    (assoc-ref inputs "jdk") "/bin/java"
                                    " -cp " out "/share/java/josm.jar:"
                                    (getenv "CLASSPATH")
                                    " org.openstreetmap.josm.gui.MainApplication"))))
               (chmod (string-append bin "/josm") #o755))
             #t)))))
    (home-page "https://josm.openstreetmap.de")
    (synopsis "OSM editor")
    (description "JOSM is an extensible editor for OpenStreetMap (OSM).  It
supports loading GPX tracks, background imagery and OSM data from local
sources as well as from online sources and allows to edit the OSM data (nodes,
ways, and relations) and their metadata tags.")
    (license license:gpl2+)))

(define-public libmaxminddb
  (package
    (name "libmaxminddb")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/maxmind/libmaxminddb"
                           "/releases/download/" version "/"
                           "/libmaxminddb-" version ".tar.gz"))
       (sha256
        (base32 "0mnimbaxnnarlw7g1rh8lpxsyf7xnmzwcczcc3lxw8xyf6ljln6x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-cc-to-gcc
           (lambda _
             (setenv "CC" "gcc"))))))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://maxmind.github.io/libmaxminddb/")
    (synopsis "C library for the MaxMind DB file format")
    (description "The libmaxminddb library provides a C library for reading
MaxMind DB files, including the GeoIP2 databases from MaxMind.  The MaxMind DB
format is a custom, but open, binary format designed to facilitate fast
lookups of IP addresses while allowing flexibility in the type of data
associated with an address.")
    (license license:asl2.0)))

(define-public python-maxminddb
  (package
    (name "python-maxminddb")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "maxminddb" version))
       (sha256
        (base32
         "0y9giw81k4wdmpryr4k42w50z292mf364a6vs1vxf83ksc9ig6j4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; Tests require a copy of the maxmind database
    (inputs
     `(("libmaxminddb" ,libmaxminddb)))
    (home-page "https://www.maxmind.com/")
    (synopsis "Reader for the MaxMind DB format")
    (description "MaxMind DB is a binary file format that stores data indexed
by IP address subnets (IPv4 or IPv6).  This is a Python module for reading
MaxMind DB files.")
    (license license:asl2.0)))

(define-public python-geoip2
  (package
    (name "python-geoip2")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "geoip2" version))
       (sha256
        (base32
         "1w7cay5q6zawjzivqbwz5cqx1qbdjw6kbriccb7l46p7b39fkzzp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; Tests require a copy of the maxmind database
    (inputs
     `(("python-maxminddb" ,python-maxminddb)
       ("python-requests" ,python-requests)))
    (home-page "https://www.maxmind.com/")
    (synopsis "MaxMind GeoIP2 API")
    (description "Provides an API for the GeoIP2 web services and databases.
The API also works with MaxMind’s free GeoLite2 databases.")
    (license license:asl2.0)))

(define-public routino
  (package
   (name "routino")
   (version "3.3.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.routino.org/download/routino-"
                         version ".tgz"))
     (sha256
      (base32
       "1ccx3s99j8syxc1gqkzsaqkmyf44l7h3adildnc5iq2md7bp8wab"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("perl" ,perl)))
   (inputs
    `(("bzip2" ,bzip2)
      ("xz" ,xz)
      ("zlib" ,zlib)))
   (arguments
    `(#:test-target "test"
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "Makefile.conf"
              (("prefix=/usr/local")
               (string-append "prefix=" (assoc-ref outputs "out")))
              (("LDFLAGS_LDSO=-Wl,-R\\.")
               "LDFLAGS_LDSO=-Wl,-R$(libdir)")
              (("#CFLAGS\\+=-DUSE_XZ")
               "CFLAGS+=-DUSE_XZ")
              (("#LDFLAGS\\+=-llzma")
               "LDFLAGS+=-llzma"))
            #t)))))
   (synopsis "Routing application for OpenStreetMap data")
   (description
    "Routino is an application for finding a route between two points
using the dataset of topographical information collected by
@url{https://www.OpenStreetMap.org}.")
   (home-page "https://www.routino.org/")
   (license license:agpl3+)))

(define-public qmapshack
  (package
    (name "qmapshack")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Maproom/qmapshack.git")
             (commit (string-append "V_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07c2hrq9sn456w7l3gdr599rmjfv2k6mh159zza7p1py8r7ywksa"))))
    (build-system qt-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("gdal" ,gdal)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("proj" ,proj)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtlocation" ,qtlocation)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)
       ("quazip" ,quazip)
       ("routino" ,routino)
       ("sqlite" ,sqlite-with-column-metadata) ; See wrap phase
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cmake-modules
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("find_package\\(Qt5PrintSupport        REQUIRED\\)" all)
                (string-append all "\nfind_package(Qt5Positioning REQUIRED)")))
             (substitute* "cmake/Modules/FindROUTINO.cmake"
               (("/usr/local")
                (assoc-ref inputs "routino")))
             ;; The following fixes are included as patches in the sources
             ;; of QMapShack, but they are not applied by default, for
             ;; some reason...
             (invoke "patch" "-p1" "-i" "FindPROJ4.patch")
             (invoke "patch" "-p1" "-i" "FindQuaZip5.patch")
             #t))
         (add-after 'install 'wrap
           ;; The program fails to run with the error:
           ;;   undefined symbol: sqlite3_column_table_name16
           ;; Forcing the program to use sqlite-with-column-metadata instead
           ;; of sqlite using LD_LIBRARY_PATH solves the problem.
           ;;
           ;; The program also fails to find the QtWebEngineProcess program,
           ;; so we set QTWEBENGINEPROCESS_PATH to help it.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (qtwebengineprocess (string-append
                                        (assoc-ref inputs "qtwebengine")
                                        "/lib/qt5/libexec/QtWebEngineProcess"))
                   (sqlite-lib (string-append (assoc-ref inputs "sqlite")
                                              "/lib")))
               (for-each (lambda (program)
                           (wrap-program program
                             `("LD_LIBRARY_PATH" ":" prefix (,sqlite-lib))
                             `("QTWEBENGINEPROCESS_PATH" =
                               (,qtwebengineprocess))))
                         (find-files bin ".*")))
             #t)))))
    (synopsis "GPS mapping application")
    (description
     "QMapShack can be used to plan your next outdoor trip or to visualize and
archive all the GPS recordings of your past trips.  It is the successor of the
QLandkarte GT application.")
    (home-page "https://github.com/Maproom/qmapshack/wiki")
    (license license:gpl3+)))

(define-public readosm
  (package
    (name "readosm")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gaia-gis.it/gaia-sins/"
                           "readosm-" version ".tar.gz"))
       (sha256
        (base32 "1v20pnda67imjd70fn0zw30aar525xicy3d3v49md5cvqklws265"))))
    (build-system gnu-build-system)
    (inputs
     `(("expat" ,expat)
       ("zlib" ,zlib)))
    (synopsis "Data extractor for OpenStreetMap files")
    (description
     "ReadOSM is a library to extract valid data from within an OpenStreetMap
input file (in @code{.osm} or @code{.osm.pbf} format).")
    (home-page "https://www.gaia-gis.it/fossil/readosm/index")
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1))))
