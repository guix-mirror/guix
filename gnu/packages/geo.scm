;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017, 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system r)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml))

(define-public geos
  (package
    (name "geos")
    (version "3.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.osgeo.org/geos/geos-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1312m02xk4sp6f1xdpb9w0ic0zbxg90p5y66qnwidl5fksscf1h0"))))
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
    (version "3.28.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1yzi08a9316jplgsl2z0qzlqxhghyqcjhv0m6i94wcain4mxk1z7"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags ;; Ensure that geoclue is referred to by output.
       (list (string-append "LDFLAGS=-L"
                            (assoc-ref %build-inputs "geoclue") "/lib")
             (string-append "CFLAGS=-I"
                            (assoc-ref %build-inputs "geoclue") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                   (goa-path (string-append
                              (assoc-ref inputs "gnome-online-accounts")
                              "/lib"))
                   (webkitgtk-path (string-append
                                    (assoc-ref inputs "webkitgtk")
                                    "/lib")))
               (wrap-program (string-append out "/bin/gnome-maps")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))

                 ;; There seems to be no way to embed the path of
                 ;; libgoa-1.0.so.0, libwebkit2gtk-4.0.so.37 and
                 ;; libjavascriptcoregtk-4.0.so.18.
                 `("LD_LIBRARY_PATH" ":" prefix (,goa-path ,webkitgtk-path)))
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
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib-networking" ,glib-networking)
       ("geoclue" ,geoclue)
       ("geocode-glib" ,geocode-glib)
       ("gfbgraph" ,gfbgraph)
       ("gjs" ,gjs)
       ("glib" ,glib)
       ("gnome-online-accounts" ,gnome-online-accounts)
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
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.osgeo.org/geotiff/libgeotiff/libgeotiff-"
                           version ".tar.gz"))
       (sha256
        (base32 "0rbjqixi4c8yz19larlzq6jda0px2gpmpp9c52cyhplbjsdhsldq"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove .csv files, distributed from EPSG under a restricted
           ;; license. See LICENSE for full license text.
           (for-each delete-file (find-files "." "\\.csv$"))
           ;; Now that we have removed the csv files, we need to modify the Makefile.
           (substitute* "Makefile.in"
             (("^all-am: .*$")
              "all-am: Makefile $(LTLIBRARIES) $(HEADERS) geo_config.h\n")
             (("^install-data-am: .*$")
              "install-data-am: install-includeHEADERS"))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("libjpeg-turbo" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("proj.4" ,proj.4)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-zlib")
             (string-append "--with-jpeg")
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
    (home-page "http://proj4.org/")
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
    (home-page "http://mapnik.org/")
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
    (version "2.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "http://download.osgeo.org/gdal/" version "/gdal-"
                     version ".tar.gz"))
              (sha256
               (base32
                "1951f7b69x3d1vic0rmq92q8f4bj3hbxnxmj5jl0cc3zg0isgmdr"))
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
           "--with-pcre"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-path
           (lambda _
             (substitute* "frmts/mrf/mrf_band.cpp"
               (("\"../zlib/zlib.h\"") "<zlib.h>")))))))
    (inputs
     `(("freexl" ,freexl)
       ("geos" ,geos)
       ("giflib" ,giflib)
       ("json-c" ,json-c)
       ("libgeotiff" ,libgeotiff)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("pcre" ,pcre)
       ("zlib" ,zlib)))
    (home-page "http://www.gdal.org/")
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

(define-public postgis
  (package
    (name "postgis")
    (version "2.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.osgeo.org/postgis/source/postgis-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1hm8migjb53cymp4qvg1h20yqllmy9f7x0awv5450391i6syyqq6"))))
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
       ("libxml2" ,libxml2)
       ("pcre" ,pcre)
       ("postgresql" ,postgresql)
       ("proj.4" ,proj.4)))
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
