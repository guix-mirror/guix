;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml))

(define-public geos
  (package
    (name "geos")
    (version "3.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.osgeo.org/geos/geos-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0ak5szby29l9l0vy43dm5z2g92xzdky20q1gc1kah1fnhkgi6nh4"))))
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
    (version "3.26.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0l40l7m9dyphvasiq1jxrn6ivavs1xwzn0bzz2x1z7x73955q783"))))
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

(define-public libgeotiff
  (package
    (name "libgeotiff")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.osgeo.org/geotiff/libgeotiff/libgeotiff-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0vjy3bwfhljjx66p9w999i4mdhsf7vjshx29yc3pn5livf5091xd"))
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
              "install-data-am: install-includeHEADERS"))))))
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
