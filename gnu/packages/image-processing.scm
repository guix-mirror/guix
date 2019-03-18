;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018 Lprndn <guix@lprndn.info>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages image-processing)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

;; We use the latest snapshot of this package because the latest release is
;; from 2011 and has known vulnerabilities that cannot easily be fixed by
;; applying patches.
(define-public dcmtk
  (package
    (name "dcmtk")
    (version "3.6.1_20170228")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://dicom.offis.de/pub/dicom/offis/"
                                  "software/dcmtk/snapshot/dcmtk-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04cwfx8yrscqcd59mxk2fh6314ckayi9cp68iql5a57pf2pg5qld"))))
    (build-system gnu-build-system)
    (inputs
     `(("libtiff" ,libtiff)
       ("libpng" ,libpng)
       ("doxygen" ,doxygen)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://dcmtk.org")
    (synopsis "Libraries and programs implementing parts of the DICOM standard")
    (description "DCMTK is a collection of libraries and applications
implementing large parts the DICOM standard.  It includes software for
examining, constructing and converting DICOM image files, handling offline
media, sending and receiving images over a network connection, as well as
demonstrative image storage and worklist servers.")
    (license (license:fsf-free
              "file://COPYRIGHT"
              "A union of the Apache 2.0 licence and various non-copyleft
licences similar to the Modified BSD licence."))))

(define-public mia
  (package
    (name "mia")
    (version "2.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mia/mia/"
                                  (version-major+minor version)
                                  "/mia-" version ".tar.xz"))
              (sha256
               (base32
                "0j4nd5z7i3v199jh7hqqhwd4g7snchizkc7rhzanpvngqg91m1pb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DMIA_CREATE_NIPYPE_INTERFACES=0"
             "-DCMAKE_CXX_FLAGS=-fpermissive")))
    (inputs
     `(("boost" ,boost)
       ("dcmtk" ,dcmtk)
       ("doxygen" ,doxygen)
       ("eigen" ,eigen)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("gsl" ,gsl)
       ("gts" ,gts)
       ("hdf5" ,hdf5)
       ("itpp" ,itpp)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libxml" ,libxml2)
       ("libxml++" ,libxml++)
       ("maxflow" ,maxflow)
       ("niftilib" ,niftilib)
       ("nlopt" ,nlopt)
       ("openexr" ,openexr)
       ("python-lxml" ,python2-lxml)
       ("vtk" ,vtk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (home-page "http://mia.sourceforge.net")
    (synopsis "Toolkit for gray scale medical image analysis")
    (description "MIA provides a combination of command line tools, plug-ins,
and libraries that make it possible run image processing tasks interactively
in a command shell and to prototype using the shell's scripting language.  It
is built around a plug-in structure that makes it easy to add functionality
without compromising the original code base and it makes use of a wide variety
of external libraries that provide additional functionality.")
    (license license:gpl3+)))

(define-public vtk
  (package
    (name "vtk")
    (version "8.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://vtk.org/files/release/"
                                  (version-major+minor version)
                                  "/VTK-" version ".tar.gz"))
              (sha256
               (base32
                "1fspgp8k0myr6p2a6wkc21ldcswb4bvmb484m12mxgk1a9vxrhrl"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"           ;Build without '-g' to save space.
       ;; -DVTK_USE_SYSTEM_NETCDF:BOOL=TRUE requires netcdf_cxx
       #:configure-flags '("-DVTK_USE_SYSTEM_EXPAT:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_FREETYPE:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_HDF5:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_JPEG:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_JSONCPP:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_LIBXML2:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_OGGTHEORA:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_PNG:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_TIFF:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_ZLIB:BOOL=TRUE")
       #:tests? #f))                              ;XXX: no "test" target
    (inputs
     `(("libXt" ,libxt)
       ("xorgproto" ,xorgproto)
       ("libX11" ,libx11)
       ("libxml2" ,libxml2)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("expat" ,expat)
       ("freetype" ,freetype)
       ("hdf5" ,hdf5)
       ("jpeg" ,libjpeg)
       ("jsoncpp" ,jsoncpp)
       ("libogg" ,libogg)
       ("libtheora" ,libtheora)
       ("png" ,libpng)
       ("tiff" ,libtiff)
       ("zlib" ,zlib)))
    (home-page "https://vtk.org/")
    (synopsis "Libraries for 3D computer graphics")
    (description
     "The Visualization Toolkit (VTK) is a C++ library for 3D computer graphics,
image processing and visualization.  It supports a wide variety of
visualization algorithms including: scalar, vector, tensor, texture, and
volumetric methods; and advanced modeling techniques such as: implicit
modeling, polygon reduction, mesh smoothing, cutting, contouring, and Delaunay
triangulation.  VTK has an extensive information visualization framework, has
a suite of 3D interaction widgets, supports parallel processing, and
integrates with various databases on GUI toolkits such as Qt and Tk.")
    (license license:bsd-3)))

(define-public opencv
  (package
    (name "opencv")
    (version "3.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/opencv/opencv")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06bc61r8myym4s8im10brdjfg4wxkrvsbhhl7vr1msdan2xddzi3"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove external libraries. We have all available in Guix:
                  (delete-file-recursively "3rdparty")

                  ;; Milky icon set is non-free:
                  (delete-file-recursively "modules/highgui/src/files_Qt/Milky")

                  ;; Some jars found:
                  (for-each delete-file
                            '("modules/java/test/pure_test/lib/junit-4.11.jar"
                              "samples/java/sbt/sbt/sbt-launch.jar"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DWITH_IPP=OFF"
             "-DWITH_ITT=OFF"
             "-DWITH_CAROTENE=OFF" ; only visible on arm/aarch64
             "-DENABLE_PRECOMPILED_HEADERS=OFF"

             ;; CPU-Features:
             ;; See cmake/OpenCVCompilerOptimizations.cmake
             ;; (CPU_ALL_OPTIMIZATIONS) for a list of all optimizations
             ;; BASELINE is the minimum optimization all CPUs must support
             ;;
             ;; DISPATCH is the list of optional dispatches.
             "-DCPU_BASELINE=SSE2"

             ,@(match (%current-system)
                 ("x86_64-linux"
                  '("-DCPU_DISPATCH=NEON;VFPV3;FP16;SSE;SSE2;SSE3;SSSE3;SSE4_1;SSE4_2;POPCNT;AVX;FP16;AVX2;FMA3;AVX_512F;AVX512_SKX"
                    "-DCPU_DISPATCH_REQUIRE=SSE3,SSSE3,SSE4_1,SSE4_2,AVX,AVX2"))
                 ("armhf-linux"
                  '("-DCPU_BASELINE_DISABLE=NEON")) ; causes build failures
                 ("aarch64-linux"
                  '("-DCPU_BASELINE=NEON"
                    "-DCPU_DISPATCH=NEON;VFPV3;FP16"))
                 (_ '()))

             "-DBUILD_PERF_TESTS=OFF"
             "-DBUILD_TESTS=ON"

             (string-append "-DOPENCV_EXTRA_MODULES_PATH=" (getcwd)
                            "/opencv-contrib/modules")

             ;;Define test data:
             (string-append "-DOPENCV_TEST_DATA_PATH=" (getcwd)
                            "/opencv-extra/testdata")

             ;; Is ON by default and would try to rebuild 3rd-party protobuf,
             ;; which we had removed, which would lead to an error:
             "-DBUILD_PROTOBUF=OFF"

             ;; Rebuild protobuf files, because we have a slightly different
             ;; version than the included one. If we would not update, we
             ;; would get a compile error later:
             "-DPROTOBUF_UPDATE_FILES=ON"

             ;; xfeatures2d disabled, because it downloads extra binaries from
             ;; https://github.com/opencv/opencv_3rdparty
             ;; defined in xfeatures2d/cmake/download_{vgg|bootdesc}.cmake
             ;; Cmp this bug entry:
             ;; https://github.com/opencv/opencv_contrib/issues/1131
             "-DBUILD_opencv_xfeatures2d=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             ;; These tests fails with:
             ;; vtkXOpenGLRenderWindow (0x723990): Could not find a decent config
             ;; I think we have no OpenGL support with the Xvfb.
             (substitute* '("modules/viz/test/test_tutorial3.cpp"
                            "modules/viz/test/test_main.cpp"
                            "modules/viz/test/tests_simple.cpp"
                            "modules/viz/test/test_viz3d.cpp")
               (("(TEST\\(Viz, )([a-z].*\\).*)" all pre post)
                (string-append pre "DISABLED_" post)))

             ;; This one fails with "unknown file: Failure"
             ;; But I couldn't figure out which file was missing:
             (substitute* "../opencv-contrib/modules/face/test/test_face_align.cpp"
               (("(TEST\\(CV_Face_FacemarkKazemi, )(can_detect_landmarks\\).*)"
                 all pre post)
                (string-append pre "DISABLED_" post)))

             ;; Failure reason: Bad accuracy
             ;; Incorrect count of accurate poses [2nd case]: 90.000000 / 94.000000
             (substitute* "../opencv-contrib/modules/rgbd/test/test_odometry.cpp"
               (("(TEST\\(RGBD_Odometry_Rgbd, )(algorithmic\\).*)" all pre post)
                (string-append pre "DISABLED_" post)))
             #t))

         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "../opencv-extra")
             (mkdir "../opencv-contrib")
             (copy-recursively (assoc-ref inputs "opencv-extra")
                               "../opencv-extra")
             (invoke "tar" "xvf"
                     (assoc-ref inputs "opencv-contrib")
                     "--strip-components=1"
                     "-C" "../opencv-contrib")))

         (add-after 'set-paths 'add-ilmbase-include-path
           (lambda* (#:key inputs #:allow-other-keys)
           ;; OpenEXR propagates ilmbase, but its include files do not appear
           ;; in the CPATH, so we need to add "$ilmbase/include/OpenEXR/" to
           ;; the CPATH to satisfy the dependency on "ImathVec.h".
           (setenv "CPATH"
                   (string-append (assoc-ref inputs "ilmbase")
                                  "/include/OpenEXR"
                                  ":" (or (getenv "CPATH") "")))
           #t))
       (add-before 'check 'start-xserver
         (lambda* (#:key inputs #:allow-other-keys)
           (let ((xorg-server (assoc-ref inputs "xorg-server"))
                 (disp ":1"))
             (setenv "HOME" (getcwd))
             (setenv "DISPLAY" disp)
             ;; There must be a running X server and make check doesn't start one.
             ;; Therefore we must do it.
             (zero? (system (format #f "~a/bin/Xvfb ~a &" xorg-server disp)))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server) ; For running the tests
       ("opencv-extra"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/opencv/opencv_extra")
                  (commit version)))
           (file-name (git-file-name "opencv_extra" version))
           (sha256
            (base32 "08p5xnq8n1jw8svvz0fnirfg7q8dm3p4a5dl7527s5xj0f9qn7lp"))))
       ("opencv-contrib"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/opencv/opencv_contrib")
                  (commit version)))
           (file-name (git-file-name "opencv_contrib" version))
           (patches (search-patches "opencv-rgbd-aarch64-test-fix.patch"))
           (sha256
            (base32 "1f334glf39nk42mpqq6j732h3ql2mpz89jd4mcl678s8n73nfjh2"))))))
    (inputs `(("libjpeg" ,libjpeg)
              ("libpng" ,libpng)
              ("jasper" ,jasper)
              ;; ffmpeg 4.0 causes core dumps in tests.
              ("ffmpeg" ,ffmpeg-3.4)
              ("libtiff" ,libtiff)
              ("hdf5" ,hdf5)
              ("libgphoto2" ,libgphoto2)
              ("libwebp" ,libwebp)
              ("zlib" ,zlib)
              ("gtkglext" ,gtkglext)
              ("openexr" ,openexr)
              ("ilmbase" ,ilmbase)
              ("gtk+" ,gtk+-2)
              ("python-numpy" ,python-numpy)
              ("protobuf" ,protobuf)
              ("vtk" ,vtk)
              ("python" ,python)))
    ;; These three CVEs are not a problem of OpenCV, see:
    ;; https://github.com/opencv/opencv/issues/10998
    (properties '((lint-hidden-cve . ("CVE-2018-7712"
                                      "CVE-2018-7713"
                                      "CVE-2018-7714"))))
    (synopsis "Computer vision library")
    (description "OpenCV is a library aimed at
real-time computer vision, including several hundred computer
vision algorithms.  It can be used to do things like:

@itemize
@item image and video input and output
@item image and video processing
@item displaying
@item feature recognition
@item segmentation
@item facial recognition
@item stereo vision
@item structure from motion
@item augmented reality
@item machine learning
@end itemize\n")
    (home-page "https://opencv.org/")
    (license license:bsd-3)))

(define-public vips
  (package
    (name "vips")
    (version "8.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/libvips/libvips/releases/download/v"
             version "/vips-" version ".tar.gz"))
       (sha256
        (base32 "01gjhcrl6zj7mcj1al717v5jsniahplqhz1xkfh2j78vyfl1hxff"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("glib" ,glib)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libtiff" ,libtiff)
       ("libexif" ,libexif)
       ("giflib" ,giflib)
       ("libgsf" ,libgsf)
       ("fftw" ,fftw)
       ("poppler" ,poppler)
       ("pango" ,pango)
       ("lcms" ,lcms)
       ("matio" ,matio)
       ("libwebp" ,libwebp)
       ("niftilib" ,niftilib)
       ("openexr" ,openexr)
       ("orc" ,orc)
       ("imagemagick" ,imagemagick)
       ("libxml2" ,libxml2)
       ("expat" ,expat)
       ("hdf5" ,hdf5)))
    (home-page "https://libvips.github.io/libvips/")
    (synopsis "Multithreaded image processing system with low memory needs")
    (description
     "VIPS is a demand-driven, horizontally threaded image processing library.
It's particularly good at processing large images, working with colour,
scientific analysis, and general research & development.

Compared to most image processing libraries VIPS needs little RAM and runs
quickly, especially on machines with more than one CPU core.  This is primarily
due to its architecture which automatically parallelises the image workflows.")
    (license license:lgpl2.1+)))

(define-public nip2
  (package
    (name "nip2")
    (version "8.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libvips/nip2/releases/download/v"
                           version "/nip2-" version ".tar.gz" ))
       (sha256
        (base32 "0l7n427njif53npqn02gfjjly8y3khbrkzqxp10j5vp9h97psgiw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; test_conv.ws keep failing so disabling for now.
         (add-after 'unpack 'disable-test-conv
           (lambda _
             (delete-file "test/workspaces/test_conv.ws")
             #t))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" "/tmp") #t)))))
    (inputs
     `(("vips" ,vips)
       ("glib" ,glib)
       ("libtiff" ,libtiff)
       ("gtk+-2" ,gtk+-2)
       ("libxml2" ,libxml2)
       ("libexif" ,libexif)
       ("libjpeg" ,libjpeg)             ; required by vips.pc
       ("librsvg" ,librsvg)
       ("fftw" ,fftw)
       ("libgsf" ,libgsf)
       ("imagemagick" ,imagemagick)
       ("orc" ,orc)
       ("matio" ,matio)
       ("lcms" ,lcms)
       ("libwebp" ,libwebp)
       ("openexr" ,openexr)
       ("poppler" ,poppler)
       ("gsl" ,gsl)))
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/libvips/nip2")
    (synopsis "Spreadsheet-like GUI for libvips")
    (description "This package provide a graphical user interface (GUI) for
the VIPS image processing library.  It's a little like a spreadsheet: you
create a set of formula connecting your objects together, and on a change nip2
recalculates.")
    (license license:gpl2+)))
