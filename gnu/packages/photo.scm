;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2017 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Sebastian Schott <sschott@mailbox.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020. 2021 Vinicius Monego <monego@posteo.net>
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

(define-module (gnu packages photo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xfig)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26))

(define-public rapid-photo-downloader
  (package
    (name "rapid-photo-downloader")
    (version "0.9.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/rapid/pyqt/"
                                  version "/+download/" name "-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15p7sssg6vmqbm5xnc4j5dr89d7gl7y5qyq44a240yl5aqkjnybw"))))
    (build-system python-build-system)
    (native-inputs
     `(("file" ,file)
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("gexiv2" ,gexiv2)
       ("gst-libav" ,gst-libav)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gstreamer" ,gstreamer)
       ("libgudev" ,libgudev)
       ("libnotify" ,libnotify)
       ("libmediainfo" ,libmediainfo)
       ("usdisks" ,udisks)
       ("python-pyqt" ,python-pyqt)
       ("python-pygobject" ,python-pygobject)
       ("python-gphoto2" ,python-gphoto2)
       ("python-pyzmq" ,python-pyzmq)
       ("python-tornado" ,python-tornado)
       ("python-psutil" ,python-psutil)
       ("python-pyxdg" ,python-pyxdg)
       ("python-arrow" ,python-arrow)
       ("python-dateutil" ,python-dateutil)
       ("python-easygui" ,python-easygui)
       ("python-colour" ,python-colour)
       ("python-pymediainfo" ,python-pymediainfo)
       ("python-sortedcontainers" ,python-sortedcontainers)
       ("python-rawkit" ,python-rawkit)
       ("python-requests" ,python-requests)
       ("python-colorlog" ,python-colorlog)
       ("python-pyprind" ,python-pyprind)
       ("python-tenacity" ,python-tenacity)
       ("perl-image-exiftool" ,perl-image-exiftool)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-libmediainfo
           (lambda _
             (substitute* "raphodo/metadatavideo.py"
               (("pymedia_library_file = 'libmediainfo.so.0'")
                (string-append "pymedia_library_file = '"
                               (assoc-ref %build-inputs "libmediainfo")
                               "/lib/libmediainfo.so.0'")))
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (path              (string-join
                                       (list (string-append
                                              (assoc-ref inputs "perl-image-exiftool")
                                              "/bin"))
                                       ":"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (python-path       (getenv "PYTHONPATH")))
               (for-each
                (lambda (program)
                  (wrap-program program
                    `("PATH" ":" prefix (,path))
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                    `("PYTHONPATH"             ":" prefix (,python-path))))
                (map (lambda (name)
                       (string-append out "/bin/" name))
                     '("analyze-pv-structure"
                       "rapid-photo-downloader"))))
             #t)))))
    (home-page "https://www.damonlynch.net/rapid/")
    (synopsis "Import photos and videos from cameras, phones and memory cards")
    (description "Import photos and videos from cameras, phones and memory
cards and generate meaningful file and folder names.")
    (license license:gpl2+)))

(define-public libraw
  (package
    (name "libraw")
    (version "0.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.libraw.org/data/LibRaw-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "18wlsvj6c1rv036ph3695kknpgzc3lk2ikgshy8417yfl8ykh2hz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libjpeg" ,libjpeg-turbo)))     ;for lossy DNGs and old Kodak cameras
    (propagated-inputs
     `(("lcms" ,lcms)))                 ;for color profiles
    (home-page "https://www.libraw.org")
    (synopsis "Raw image decoder")
    (description
     "LibRaw is a library for reading RAW files obtained from digital photo
cameras (CRW/CR2, NEF, RAF, DNG, and others).")
    ;; LibRaw is distributed under both LGPL2.1 and CDDL 1.0.  From the README:
    ;; "You may use one of these licensing modes and switch between them.  If
    ;; you modify LibRaw source and made your changes public, you should accept
    ;; both two licensing modes for your changes/additions."
    (license (list license:lgpl2.1 license:cddl1.0))))

(define-public libraw-0.18
  (package (inherit libraw)
    (name "libraw")
    (version "0.18.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.libraw.org/data/LibRaw-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1m2khr2cij8z6lawgbmdksjn14fpnjsy8ad4qahnpqapm1slsxap"))))))

(define-public libexif
  (package
    (name "libexif")
    (version "0.6.22")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libexif/libexif/releases"
                    "/download/libexif-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    "-release/libexif-" version ".tar.xz"))
              (sha256
               (base32
                "0mhcad5zab7fsn120rd585h8ncwkq904nzzrq8vcd72hzk4g2j2h"))))
    (build-system gnu-build-system)
    (home-page "https://libexif.github.io/")
    (synopsis "Read and manipulate EXIF data in digital photographs")
    (description
     "The libexif C library allows applications to read, edit, and save EXIF
data as produced by digital cameras.")
    (license license:lgpl2.1+)))

(define-public libgphoto2
  (package
    (name "libgphoto2")
    (version "2.5.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gphoto/libgphoto/"
                                  version "/libgphoto2-" version ".tar.bz2"))
              (sha256
               (base32
                "1ms06b3dj1p33aypcb16gg5pn7fylbylsk9cnnqa0j29qiw59f7q"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libjpeg-turbo" ,libjpeg-turbo)
       ("libltdl" ,libltdl)
       ("libusb" ,libusb)
       ("libxml2" ,libxml2)))
    (propagated-inputs
     `(;; The .pc refers to libexif.
       ("libexif" ,libexif)))
    (home-page "http://www.gphoto.org/proj/libgphoto2/")
    (synopsis "Accessing digital cameras")
    (description
     "This is the library backend for gphoto2.  It contains the code for PTP,
MTP, and other vendor specific protocols for controlling and transferring data
from digital cameras.")

    ;; 'COPYING' says LGPLv2.1+, but in practices files are under LGPLv2+.
    (license license:lgpl2.1+)))

(define-public gphoto2
  (package
    (name "gphoto2")
    (version "2.5.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gphoto/gphoto/" version
                                  "/gphoto2-" version ".tar.bz2"))
              (sha256
               (base32
                "0f4d3q381jnnkcqkb2dj1k709skp65qihl5xm80zandvl69lw19h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("readline" ,readline)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("popt" ,popt)
       ("libexif" ,libexif)
       ("libgphoto2" ,libgphoto2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "tests/data" "\\.param$")
               (("/usr/bin/env")
                (which "env")))
             #t)))

       ;; FIXME: There is 1 test failure, most likely related to the build
       ;; environment.
       #:tests? #f))

    (home-page "http://www.gphoto.org/")
    (synopsis "Command-line tools to access digital cameras")
    (description
     "Gphoto2 is a set of command line utilities for manipulating a large
number of different digital cameras.  Through libgphoto2, it supports PTP,
MTP, and much more.")

    ;; Files are typically under LGPLv2+, but 'COPYING' says GPLv2+.
    (license license:gpl2+)))

;; Note: See <https://metacpan.org/pod/Image::ExifTool> for the latest
;; release.  The versions at <https://www.sno.phy.queensu.ca/~phil/exiftool/>
;; are not meant for production use according to the Changes file.
(define-public perl-image-exiftool
  (package
    (name "perl-image-exiftool")
    (version "12.00")
    (source
     (origin
       (method url-fetch)
       (uri (list
             (string-append "mirror://cpan/authors/id/E/EX/EXIFTOOL/"
                            "Image-ExifTool-" version ".tar.gz")
             ;; New releases may take a while to hit CPAN.
             (string-append "https://www.sno.phy.queensu.ca/~phil/exiftool/"
                            "Image-ExifTool-" version ".tar.gz")))
       (sha256
        (base32
         "0nl5djf6hs6brnp7qnqvj3xwhj1qnjwcv35ih4yqp2mm9b4jqyfh"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure the 'exiftool' commands finds the library.
             ;; XXX: Shouldn't it be handled by PERL-BUILD-SYSTEM?
             (let* ((out (assoc-ref outputs "out"))
                    (pm  (find-files out "^ExifTool\\.pm$"))
                    (lib (dirname (dirname (car pm)))))
               (wrap-program (string-append out "/bin/exiftool")
                 `("PERL5LIB" prefix (,lib)))
               #t))))))
    (home-page "https://metacpan.org/release/Image-ExifTool")
    (synopsis "Program and Perl library to manipulate EXIF and other metadata")
    (description "This package provides the @code{exiftool} command and the
@code{Image::ExifTool} Perl library to manipulate EXIF tags of digital images
and a wide variety of other metadata.")
    (license license:perl-license)))

(define-public libpano13
  (package
    (name "libpano13")
    (version "2.9.20_rc3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/panotools/libpano13/"
                                  "libpano13-" (first
                                                (string-split version #\_))
                                  "/libpano13-" version ".tar.gz"))
              (sha256
               (base32
                "12cv4886l1czfjwy7k6ipgf3zjksgwhdjzr2s9fdg33vqcv2hlrv"))))
    (build-system cmake-build-system)
    (inputs
     `(("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("zlib" ,zlib)))
    (home-page "http://panotools.sourceforge.net/")
    (synopsis "Library for panoramic images")
    (description
     "The libpano13 package contains the backend library written by the
Panorama Tools project for building panoramic images from a set of
overlapping images, as well as some command line tools.")
    (license license:gpl2+)))

(define-public enblend-enfuse
  (package
    (name "enblend-enfuse")
    (version "4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/enblend/"
                                  name "/"
                                  name "-" (version-major+minor version) "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0j5x011ilalb47ssah50ag0a4phgh1b0wdgxdbbp1gcyjcjf60w7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("perl-timedate" ,perl-timedate)
       ;; for building the documentation
       ("gnuplot" ,gnuplot)
       ("help2man" ,help2man)
       ("imagemagick" ,imagemagick)
       ("libxml2" ,libxml2)
       ("texlive-minimal" ,texlive-tiny)
       ("tidy" ,tidy)
       ("transfig" ,transfig)))
    (inputs
     `(("boost" ,boost)
       ("gsl" ,gsl)
       ("lcms" ,lcms)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("openexr" ,openexr)
       ("vigra" ,vigra)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags `("--enable-openmp")))
    (home-page "http://enblend.sourceforge.net/")
    (synopsis "Tools for combining and blending images")
    (description
     "Enblend blends away the seams in a panoramic image mosaic using a
multi-resolution spline.  Enfuse merges different exposures of the same
scene to produce an image that looks much like a tone-mapped image.")
    (license license:gpl2+)))

(define-public lensfun
  (package
    (name "lensfun")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/lensfun/"
                    version "/lensfun-" version ".tar.gz"))
              (sha256
               (base32
                "0cfk8jjhs9nbfjfdy98plrj9ayi59aph0nx6ppslgjhlcvacm2xf"))))
    (build-system cmake-build-system)
    (arguments
     `(,@(if (any (cute string-prefix? <> (or (%current-system)
                                              (%current-target-system)))
                  '("x86_64" "i686"))
        ;; SSE and SSE2 are supported only on Intel processors.
        '()
        '(#:configure-flags '("-DBUILD_FOR_SSE=OFF" "-DBUILD_FOR_SSE2=OFF")))
       #:tests? #f)) ; There are no tests to run.
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://sourceforge.net/projects/lensfun/")
    (synopsis "Library to correct optical lens defects with a lens database")
    (description "Digital photographs are not ideal.  Of course, the better is
your camera, the better the results will be, but in any case if you look
carefully at shots taken even by the most expensive cameras equipped with the
most expensive lenses you will see various artifacts.  It is very hard to make
ideal cameras, because there are a lot of factors that affect the final image
quality, and at some point camera and lens designers have to trade one factor
for another to achieve the optimal image quality, within the given design
restrictions and budget.  But we all want ideal shots, don't we?  So that's
what's Lensfun is all about: rectifying the defects introduced by your
photographic equipment.")
    ;; The libraries are licensed under the LGPL3, the programs are
    ;; licensed GPL3, and the database is license CC-BY-SA 3.0.  See the
    ;; README.md file for this clarification.
    (license (list license:lgpl3 license:gpl3 license:cc-by-sa3.0))))

(define-public darktable
  (package
    (name "darktable")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/darktable-org/darktable/releases/"
             "download/release-" version "/darktable-" version ".tar.xz"))
       (sha256
        (base32 "07llfhhz5dhh43smhv4ax4xi1diym8hrzl7cad87rkcvv98zihvz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBINARY_PACKAGE_BUILD=On"
                           "-DBUILD_TESTING=On")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'prepare-build-environment
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Rawspeed fails to build with GCC due to OpenMP error:
             ;; "undefined reference to `GOMP_loop_nonmonotonic_dynamic_next'"
             (setenv "CC" "clang") (setenv "CXX" "clang++")
             ;; Darktable looks for opencl-c.h in the LLVM dir. Guix installs
             ;; it to the Clang dir. We fix this by patching CMakeLists.txt.
             (substitute* "CMakeLists.txt"
               (("\\$\\{LLVM_INSTALL_PREFIX\\}")
                (assoc-ref %build-inputs "clang")))
             #t))
         (add-before 'configure 'set-LDFLAGS-and-CPATH
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "LDFLAGS"
                     (string-append
                      "-Wl,-rpath="
                      (assoc-ref outputs "out") "/lib/darktable"))

             ;; Ensure the OpenEXR headers are found.
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "ilmbase")
                                    "/include/OpenEXR:" (or (getenv "CPATH") "")))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/darktable")
               ;; For GtkFileChooserDialog.
               `("GSETTINGS_SCHEMA_DIR" =
                 (,(string-append (assoc-ref inputs "gtk+")
                                  "/share/glib-2.0/schemas")))
               ;; For libOpenCL.so.
               `("LD_LIBRARY_PATH" =
                 (,(string-append (assoc-ref inputs "ocl-icd")
                                  "/lib"))))
             #t)))))
    (native-inputs
     `(("clang" ,clang-11)
       ("cmocka" ,cmocka)
       ("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("llvm" ,llvm-11) ;should match the Clang version
       ("opencl-headers" ,opencl-headers)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("po4a" ,po4a)
       ("python" ,python-wrapper)
       ("ruby" ,ruby)))
    (inputs
     `(("cairo" ,cairo)
       ("colord-gtk" ,colord-gtk) ;optional, for color profile support
       ("cups" ,cups) ;optional, for printing support
       ("curl" ,curl)
       ("dbus-glib" ,dbus-glib)
       ("exiv2" ,exiv2)
       ("freeimage" ,freeimage)
       ("gmic" ,gmic) ;optional, for HaldcLUT support
       ("graphicsmagick" ,graphicsmagick)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("ilmbase" ,ilmbase)
       ("iso-codes" ,iso-codes) ;optional, for language names in the preferences
       ("json-glib" ,json-glib)
       ("lcms" ,lcms)
       ("lensfun" ,lensfun) ;optional, for the lens distortion plugin
       ("libgphoto2" ,libgphoto2) ;optional, for camera tethering
       ("libavif" ,libavif) ;optional, for AVIF support
       ("libjpeg" ,libjpeg-turbo)
       ("libomp" ,libomp)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libsecret" ,libsecret) ;optional, for storing passwords
       ("libsoup" ,libsoup)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp) ;optional, for WebP support
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("lua" ,lua) ;optional, for plugins
       ("ocl-icd" ,ocl-icd) ;optional, for OpenCL support
       ("openexr" ,openexr) ;optional, for EXR import/export
       ("openjpeg" ,openjpeg) ;optional, for JPEG2000 export
       ("osm-gps-map" ,osm-gps-map) ;optional, for geotagging view
       ("pugixml" ,pugixml)
       ("python-jsonschema" ,python-jsonschema)
       ("sqlite" ,sqlite)))
    (home-page "https://www.darktable.org")
    (synopsis "Virtual lighttable and darkroom for photographers")
    (description "Darktable is a photography workflow application and RAW
developer.  It manages your digital negatives in a database, lets you view
them through a zoomable lighttable and enables you to develop raw images
and enhance them.")
    ;; See src/is_supported_platform.h for supported platforms.
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (license (list license:gpl3+ ;; Darktable itself.
                   license:lgpl2.1+)))) ;; Rawspeed library.

(define-public photoflare
  (package
    (name "photoflare")
    (version "1.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/photoflare/photoflare")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rh5gvnc1zwx4p9h599s82m69gsxp19nnfcxsblx3b2ddwzxh78v"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((magickpp (assoc-ref inputs "graphicsmagick"))
                   (out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "INCLUDEPATH += " magickpp
                                      "/include/GraphicsMagick")
                       (string-append "PREFIX=" out)
                       "Photoflare.pro")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("graphicsmagick" ,graphicsmagick)
       ("libomp" ,libomp)
       ("qtbase" ,qtbase)))
    (home-page "https://photoflare.io")
    (synopsis "Quick, simple but powerful image editor")
    (description "Photoflare is a cross-platform image editor with an aim
to balance between powerful features and a very friendly graphical user
interface.  It suits a wide variety of different tasks and users who value a
more nimble workflow.  Features include basic image editing capabilities,
paint brushes, image filters, colour adjustments and more advanced features
such as Batch image processing.")
    (license license:gpl3+)))

(define-public entangle
  (package
    (name "entangle")
    (version "3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/entangle/entangle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pdmgxjdb3xlcqsaz7l8qzj5f7g7nwzhsrgid8929bm36d49cgc7"))))
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
         (add-after 'install 'wrap-gi-python
           ;; Make GTK find files needed by plugins.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (python-path       (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/bin/entangle")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("PYTHONPATH" ":" prefix (,python-path))))
             #t)))))
    (native-inputs
     `(("cmake" ,cmake)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("gexiv2" ,gexiv2)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gstreamer" ,gstreamer)
       ("gtk+" ,gtk+)
       ("lcms" ,lcms)
       ("libgphoto2" ,libgphoto2)
       ("libgudev" ,libgudev)
       ("libpeas" ,libpeas)
       ("libraw" ,libraw)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://entangle-photo.org/")
    (synopsis "Camera control and capture")
    (description
     "Entangle is an application which uses GTK and libgphoto2 to provide a
graphical interface for tethered photography with digital cameras.  It
includes control over camera shooting and configuration settings and 'hands
off' shooting directly from the controlling computer.")
    (license license:gpl3+)))

(define-public hugin
  (package
    (name "hugin")
    (version "2020.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/hugin/hugin/hugin-"
                                  (version-major+minor version)
                                  "/hugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1jyazc0mbr9g7vrichpqqnfl72lj21244csk0z5i8ycs4l0pcgi8"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("enblend-enfuse" ,enblend-enfuse)
       ("exiv2" ,exiv2)
       ("fftw" ,fftw)
       ("flann" ,flann)
       ("freeglut" ,freeglut)
       ("glew" ,glew)
       ("lcms" ,lcms)
       ("libjpeg" ,libjpeg-turbo)
       ("libpano13" ,libpano13)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libxi" ,libxi)
       ("libxmu" ,libxmu)
       ("mesa" ,mesa)
       ("openexr" ,openexr)
       ("sqlite" ,sqlite)
       ("vigra" ,vigra)
       ("wxwidgets" ,wxwidgets)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f                      ; no check target
       #:configure-flags
       (list
        ;; The header files of ilmbase (propagated by openexr) are not found
        ;; when included by the header files of openexr, and an explicit
        ;; flag needs to be set.
        (string-append "-DCMAKE_CXX_FLAGS=-I"
                       (assoc-ref %build-inputs "ilmbase")
                       "/include/OpenEXR")
        ;; Disable installation of the Python scripting interface.
        ;; It would require the additional inputs python and swig.
        ;; Installation would need to be tweaked, as it tries to install
        ;; into the python directory.
        "-DBUILD_HSI=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'substitute
           (lambda _
             (substitute* "src/hugin1/base_wx/StitchingExecutor.cpp"
               (("wxT\\(\"enblend\"\\)")
                (string-append "wxT(\"" (which "enblend") "\")"))
               (("wxT\\(\"enfuse\"\\)")
                (string-append "wxT(\"" (which "enfuse") "\")")))
             #t)))))
    (home-page "http://hugin.sourceforge.net/")
    (synopsis "Panorama photo stitcher")
    (description
     "Hugin is an easy to use panoramic imaging toolchain with a graphical
user interface.  It can be used to assemble a mosaic of photographs into
a complete panorama and stitch any series of overlapping pictures.")
    (license license:gpl2+)))

(define-public rawtherapee
  (package
    (name "rawtherapee")
    (version "5.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://rawtherapee.com/shared/source/"
                                  "rawtherapee-" version ".tar.xz"))
              (sha256
               (base32
                "0lq8qi7g0a28h3rab7bk5bbbd4gvfma42bvlz1dfn8p9mah2h19n"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; no test suite
       #:build-type "release"
       #:configure-flags
       (list (string-append "-DLENSFUNDBDIR="
                            (assoc-ref %build-inputs "lensfun")
                            "/share/lensfun")
             ;; Don't optimize the build for the host machine. See the file
             ;; 'ProcessorTargets.cmake' in the source distribution for more
             ;; information.
             "-DPROC_TARGET_NUMBER=1"
             ;; These flags are recommended by upstream for distributed packages.
             ;; See the file 'RELEASE_NOTES.txt' in the source distribution.
             "-O3"
             "-DCACHE_NAME_SUFFIX=\"\"")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("expat" ,expat)
       ("fftw" ,fftwf)
       ("glib" ,glib)
       ("glibmm" ,glibmm)
       ("gtk+" ,gtk+)
       ("gtkmm" ,gtkmm)
       ("lcms" ,lcms)
       ("lensfun" ,lensfun)
       ("libcanberra" ,libcanberra)
       ("libiptcdata" ,libiptcdata)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libsigc++" ,libsigc++)
       ("libtiff" ,libtiff)
       ("zlib" ,zlib)))
    (home-page "http://rawtherapee.com")
    (synopsis "Raw image developing and processing")
    (description "RawTherapee is a raw image processing suite.  It comprises a
subset of image editing operations specifically aimed at non-destructive raw
photo post-production and is primarily focused on improving a photographer's
workflow by facilitating the handling of large numbers of images.  Most raw
formats are supported, including Pentax Pixel Shift, Canon Dual-Pixel, and those
from Foveon and X-Trans sensors.")
    (license license:gpl3+)))
