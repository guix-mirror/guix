;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages gl)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define-public glu
  (package
    (name "glu")
    (version "9.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.freedesktop.org/pub/mesa/glu/glu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0r72yyhj09x3krn3kn629jqbwyq50ji8w5ri2pn6zwrk35m4g1s3"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa))) ; according to glu.pc
    (home-page "http://www.opengl.org/archives/resources/faq/technical/glu.htm")
    (synopsis "Mesa OpenGL Utility library")
    (description
     "GLU, or OpenGL Utility Library provides some higher-level functionality
not provided by just OpenGL itself.  Some of GLU's Features
include: Scaling of 2D images and creation of mipmap pyramids,
Transformation of object coordinates into device coordinates and
vice versa, Support for NURBS surfaces, Support for tessellation
of concave or bow tie polygonal primitives, Specialty transformation
matrices for creating perspective and orthographic projections,
positioning a camera, and selection/picking, Rendering of disk,
cylinder, and sphere primitives, Interpreting OpenGL error values
as ASCII text.")
    (license (license:x11-style "http://directory.fsf.org/wiki/License:SGIFreeBv2"))))

(define-public freeglut
  (package
    (name "freeglut")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/freeglut/freeglut/"
                    version "/freeglut-" version ".tar.gz"))
              (sha256
               (base32
                "18knkyczzwbmyg8hr4zh8a1i5ga01np2jzd1rwmsh7mh2n2vwhra"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ; no test target
    (inputs `(("mesa" ,mesa)
              ("libx11" ,libx11)
              ("libxi" ,libxi)
              ("libxrandr" ,libxrandr)
              ("libxxf86vm" ,libxxf86vm)
              ("xorgproto" ,xorgproto)
              ("xinput" ,xinput)))
    (propagated-inputs
     ;; Headers from Mesa and GLU are needed.
     `(("glu" ,glu)
       ("mesa" ,mesa)))
    (home-page "http://freeglut.sourceforge.net/")
    (synopsis "Alternative to the OpenGL Utility Toolkit (GLUT)")
    (description
     "Freeglut is a completely Free/OpenSourced alternative to
the OpenGL Utility Toolkit (GLUT) library.  GLUT was originally
written by Mark Kilgard to support the sample programs in the
second edition OpenGL 'RedBook'.  Since then, GLUT has been used
in a wide variety of practical applications because it is simple,
widely available and highly portable.

GLUT (and hence freeglut) allows the user to create and manage windows
containing OpenGL contexts on a wide range of platforms and also read
the mouse, keyboard and joystick functions.  Freeglut is released under
the X-Consortium license.")
    (license license:x11)))

;; Needed for "kiki".
(define-public freeglut-2.8
  (package (inherit freeglut)
    (name "freeglut")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/freeglut/freeglut/"
                    version "/freeglut-" version ".tar.gz"))
              (sha256
               (base32
                "16lrxxxd9ps9l69y3zsw6iy0drwjsp6m26d1937xj71alqk6dr6x"))))
    (build-system gnu-build-system)))

(define-public ftgl
  (package
    (name "ftgl")
    (version "2.1.3-rc5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/ftgl/FTGL%20Source/2.1.3~rc5/"
                    "ftgl-" version ".tar.gz"))
              (sha256
               (base32
                "0nsn4s6vnv5xcgxcw6q031amvh2zfj2smy1r5mbnjj2548hxcn2l"))))
    (build-system gnu-build-system)
    ;; The pkg-config file lists "freetype2" as Requires.private.
    (propagated-inputs `(("freetype" ,freetype)))
    (inputs `(("libx11" ,libx11)
              ("mesa" ,mesa)
              ("glu" ,glu)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://ftgl.sourceforge.net")
    (synopsis "Font rendering library for OpenGL applications")
    (description
     "FTGL is a font rendering library for OpenGL applications.  Supported
rendering modes are: Bitmaps, Anti-aliased pixmaps, Texture maps, Outlines,
Polygon meshes, and Extruded polygon meshes.")
    (license license:x11)))

(define-public s2tc
  (package
    (name "s2tc")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/divVerent/s2tc.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fg323fk7wlv2xh6lw66wswgcv6qi8aaadk7c28h2f2lj1s7njnf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("mesa-headers" ,mesa-headers)))
    (home-page "https://github.com/divVerent/s2tc")
    (synopsis "S3 Texture Compression implementation")
    (description
     "S2TC is a patent-free implementation of S3 Texture Compression (S3TC,
also known as DXTn or DXTC) for Mesa.")
    (license license:expat)))

;;; Mesa needs LibVA headers to build its Gallium-based VA API implementation;
;;; LibVA itself depends on Mesa.  We use the following to solve the circular
;;; dependency.
(define libva-without-mesa
  ;; Delay to work around circular import problem.
  (delay
    (package
      (inherit libva)
      (name "libva-without-mesa")
      (inputs `(,@(fold alist-delete (package-inputs libva)
                        '("mesa" "wayland"))))
      (arguments
       (strip-keyword-arguments
        '(#:make-flags)
        (substitute-keyword-arguments (package-arguments libva)
          ((#:configure-flags flags)
           '(list "--disable-glx" "--disable-egl"))))))))

(define-public mesa
  (package
    (name "mesa")
    (version "18.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (list (string-append "https://mesa.freedesktop.org/archive/"
                                  "mesa-" version ".tar.xz")
                   (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                  "mesa-" version ".tar.xz")
                   (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                  version "/mesa-" version ".tar.xz")))
        (sha256
         (base32
          "0qyw9dj2p9n91qzc4ylck2an7ibssjvzi2bjcpv2ajk851yq47sv"))
        (patches
         (search-patches "mesa-skip-disk-cache-test.patch"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(;; The following are in the Requires.private field of gl.pc.
        ("libdrm" ,libdrm)
        ("libvdpau" ,libvdpau)
        ("libx11" ,libx11)
        ("libxdamage" ,libxdamage)
        ("libxfixes" ,libxfixes)
        ("libxshmfence" ,libxshmfence)
        ("libxxf86vm" ,libxxf86vm)
        ("xorgproto" ,xorgproto)))
    (inputs
      `(("expat" ,expat)
        ("libelf" ,elfutils)  ;required for r600 when using llvm
        ("libva" ,(force libva-without-mesa))
        ("libxml2" ,libxml2)
        ;; TODO: Add 'libxml2-python' for OpenGL ES 1.1 and 2.0 support
        ("libxrandr" ,libxrandr)
        ("libxvmc" ,libxvmc)
        ,@(match (%current-system)
            ((or "x86_64-linux" "i686-linux")
             `(("llvm" ,llvm-6))) ;TODO: Change to LLVM in the next rebuild cycle.
            (_
             `()))
        ("makedepend" ,makedepend)
        ("wayland" ,wayland)
        ("wayland-protocols" ,wayland-protocols)))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ("python" ,python)
        ("python-mako" ,python-mako)
        ("which" ,(@ (gnu packages base) which))))
    (arguments
     `(#:configure-flags
       '(,@(match (%current-system)
             ((or "armhf-linux" "aarch64-linux")
              ;; TODO: Fix svga driver for aarch64 and armhf.
              '("--with-gallium-drivers=etnaviv,freedreno,imx,nouveau,pl111,r300,r600,swrast,tegra,v3d,vc4,virgl"))
             (_
              '("--with-gallium-drivers=i915,nouveau,r300,r600,radeonsi,svga,swrast,virgl")))
         ;; Enable various optional features.  TODO: opencl requires libclc,
         ;; omx requires libomxil-bellagio
         "--with-platforms=x11,drm,surfaceless,wayland"
         "--enable-glx-tls"        ;Thread Local Storage, improves performance
         ;; "--enable-opencl"
         ;; "--enable-omx"
         "--enable-osmesa"
         "--enable-xa"
         ;; features required by wayland
         "--enable-gles2"
         "--enable-gbm"
         "--enable-shared-glapi"

         ;; Enable Vulkan on i686-linux and x86-64-linux.
         ,@(match (%current-system)
             ("x86_64-linux"
              '("--with-vulkan-drivers=intel,radeon"))
             ;; TODO: Fix intel driver on i686-linux.
             ("i686-linux"
              '("--with-vulkan-drivers=radeon"))
             (_
              '("")))

         ;; Also enable the tests.
         "--enable-gallium-tests"

         ;; on non-intel systems, drop i915 and i965
         ;; from the default dri drivers
         ,@(match (%current-system)
             ((or "x86_64-linux" "i686-linux")
              '("--with-dri-drivers=i915,i965,nouveau,r200,radeon,swrast"
                "--enable-llvm"))         ; default is x86/x86_64 only
             (_
              '("--with-dri-drivers=nouveau,r200,radeon,swrast"))))
       #:modules ((ice-9 match)
                  (srfi srfi-1)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after
           'unpack 'patch-create_test_cases
           (lambda _
             (substitute* "src/intel/genxml/gen_pack_header.py"
               (("/usr/bin/env python2") (which "python")))
             #t))
         (add-before
           'build 'fix-dlopen-libnames
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Remain agnostic to .so.X.Y.Z versions while doing
               ;; the substitutions so we're future-safe.
               (substitute* "src/glx/dri_common.c"
                 (("dlopen\\(\"libGL\\.so")
                  (string-append "dlopen(\"" out "/lib/libGL.so")))
               (substitute* "src/egl/drivers/dri2/egl_dri2.c"
                 (("\"libglapi\\.so")
                  (string-append "\"" out "/lib/libglapi.so")))
               (substitute* "src/gbm/main/backend.c"
                 ;; No need to patch the gbm_gallium_drm.so reference;
                 ;; it's never installed since Mesa removed its
                 ;; egl_gallium support.
                 (("\"gbm_dri\\.so")
                  (string-append "\"" out "/lib/dri/gbm_dri.so")))
               #t)))
         (add-after 'install 'symlinks-instead-of-hard-links
           (lambda* (#:key outputs #:allow-other-keys)
             ;; All the drivers and gallium targets create hard links upon
             ;; installation (search for "hardlink each megadriver instance"
             ;; in the makefiles).  This is no good for us since we'd produce
             ;; nars that contain several copies of these files.  Thus, turn
             ;; them into symlinks, which saves ~124 MiB.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (files  (find-files lib
                                        (lambda (file stat)
                                          (and (string-contains file ".so")
                                               (eq? 'regular
                                                    (stat:type stat))))))
                    (inodes (map (compose stat:ino stat) files)))
               (for-each (lambda (inode)
                           (match (filter-map (match-lambda
                                                ((file ino)
                                                 (and (= ino inode) file)))
                                              (zip files inodes))
                             ((_)
                              #f)
                             ((reference others ..1)
                              (format #t "creating ~a symlinks to '~a'~%"
                                      (length others) reference)
                              (for-each delete-file others)
                              (for-each (lambda (file)
                                          (if (string=? (dirname file)
                                                        (dirname reference))
                                              (symlink (basename reference)
                                                       file)
                                              (symlink reference file)))
                                        others))))
                         (delete-duplicates inodes))
               #t))))))
    (home-page "https://mesa3d.org/")
    (synopsis "OpenGL and Vulkan implementations")
    (description "Mesa is a free implementation of the OpenGL and Vulkan
specifications - systems for rendering interactive 3D graphics.  A variety of
device drivers allows Mesa to be used in many different environments ranging
from software emulation to complete hardware acceleration for modern GPUs.")
    (license license:x11)))

(define-public mesa-headers
  (package
    (inherit mesa)
    (name "mesa-headers")
    (propagated-inputs '())
    (inputs '())
    (native-inputs '())
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "include" (string-append
                                          (assoc-ref outputs "out")
                                          "/include"))
             #t)))))))

;;; The mesa-demos distribution contains non-free files, many files with no
;;; clear license information, and many demos that aren't useful for most
;;; people, so we just use this for the mesa-utils package below, and possibly
;;; other packages in the future.  This is modeled after Debian's solution.
(define (mesa-demos-source version)
  (origin
    (method url-fetch)
    (uri (string-append "ftp://ftp.freedesktop.org/pub/mesa/demos"
                        "/mesa-demos-" version ".tar.bz2"))
    (sha256 (base32 "0zgzbz55a14hz83gbmm0n9gpjnf5zadzi2kjjvkn6khql2a9rs81"))))

(define-public mesa-utils
  (package
    (name "mesa-utils")
    (version "8.4.0")
    (source (mesa-demos-source version))
    (build-system gnu-build-system)
    (inputs
     `(("mesa" ,mesa)
       ("glut" ,freeglut)
       ("glew" ,glew)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/bin"))
              (for-each
               (lambda (file)
                 (copy-file file (string-append out "/bin/" (basename file))))
               '("src/xdemos/glxdemo" "src/xdemos/glxgears"
                 "src/xdemos/glxinfo" "src/xdemos/glxheads"))
              #t))))))
    (home-page "http://mesa3d.org/")
    (synopsis "Utility tools for Mesa")
    (description
     "The mesa-utils package contains several utility tools for Mesa: glxdemo,
glxgears, glxheads, and glxinfo.")
    ;; glxdemo is public domain; others expat.
    (license (list license:expat license:public-domain))))

(define-public glew
  (package
    (name "glew")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/glew/glew/" version
                                  "/glew-" version ".tgz"))
              (sha256
               (base32
                "159wk5dc0ykjbxvag5i1m2mhp23zkk6ra04l26y3jc3nwvkr3ph4"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "config/Makefile.linux"
                    (("= cc") "= gcc")
                    (("/lib64") "/lib"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list (string-append "GLEW_PREFIX="
                                         (assoc-ref %outputs "out"))
                          (string-append "GLEW_DEST="
                                         (assoc-ref %outputs "out")))
       #:tests? #f))                              ;no 'check' target
    (inputs
     `(("libxi" ,libxi)
       ("libxmu" ,libxmu)
       ("libx11" ,libx11)
       ("mesa" ,mesa)))

    ;; <GL/glew.h> includes <GL/glu.h>.
    (propagated-inputs `(("glu" ,glu)))

    (home-page "http://glew.sourceforge.net/")
    (synopsis "OpenGL extension loading library for C and C++")
    (description
     "The OpenGL Extension Wrangler Library (GLEW) is a C/C++ extension
loading library.  GLEW provides efficient run-time mechanisms for determining
which OpenGL extensions are supported on the target platform.  OpenGL core and
extension functionality is exposed in a single header file.")
    (license license:bsd-3)))

(define-public guile-opengl
  (package
    (name "guile-opengl")
    (version "0.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/guile-opengl/guile-opengl-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "13qfx4xh8baryxqrv986l848ygd0piqwm6s2s90pxk9c0m9vklim"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.2)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("freeglut" ,freeglut)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                 (add-after 'configure 'patch-makefile
                   (lambda _
                     ;; Install compiled Guile files in the expected place.
                     (substitute* '("Makefile")
                       (("^godir = .*$")
                        "godir = $(moddir)\n"))))
                 (add-before 'build 'patch-dynamic-link
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (define (dynamic-link-substitute file lib input)
                       (substitute* file
                         (("dynamic-link \"lib([a-zA-Z]+)\"" _ lib)
                          (string-append "dynamic-link \""
                                         (assoc-ref inputs input)
                                         "/lib/lib" lib "\""))))
                     ;; Replace dynamic-link calls for libGL, libGLU, and
                     ;; libglut with absolute paths to the store.
                     (dynamic-link-substitute "glx/runtime.scm" "GL" "mesa")
                     (dynamic-link-substitute "glu/runtime.scm" "GLU" "glu")
                     (dynamic-link-substitute "glut/runtime.scm" "glut"
                                              "freeglut"))))))
    (home-page "https://gnu.org/s/guile-opengl")
    (synopsis "Guile binding for the OpenGL graphics API")
    (description
     "Guile-OpenGL is a library for Guile that provides bindings to the
OpenGL graphics API.")
    (license license:lgpl3+)))

(define-public libepoxy
  (package
    (name "libepoxy")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/anholt/libepoxy/releases/download/"
                    version "/libepoxy-" version ".tar.xz"))
              (sha256
               (base32
                "0ga3qjv50x37my6pw5xr14g5n6z78hy5s8s06kays8c3ab2mha80"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (add-before
           'configure 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python"))
                   (mesa (assoc-ref inputs "mesa")))
               (substitute* "src/gen_dispatch.py"
                 (("/usr/bin/env python") python))
               (substitute* (find-files "." "\\.[ch]$")
                 (("libGL.so.1") (string-append mesa "/lib/libGL.so.1"))
                 (("libEGL.so.1") (string-append mesa "/lib/libEGL.so.1")))
               #t))))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     `(("mesa" ,mesa)))
    (home-page "https://github.com/anholt/libepoxy/")
    (synopsis "A library for handling OpenGL function pointer management")
    (description
     "A library for handling OpenGL function pointer management.")
    (license license:x11)))

(define-public soil
  (package
    (name "soil")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              ;; No versioned archive available.
              (uri "http://www.lonesock.net/files/soil.zip")
              (sha256
               (base32
                "00gpwp9dldzhsdhksjvmbhsd2ialraqbv6v6dpikdmpncj6mnc52"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'init-build
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (setenv "CFLAGS" "-fPIC") ; needed for shared library
                        ;; Use alternate Makefile
                        (copy-file "projects/makefile/alternate Makefile.txt"
                                   "src/Makefile")
                        (chdir "src")
                        (substitute* '("Makefile")
                          (("INCLUDEDIR = /usr/include/SOIL")
                           (string-append "INCLUDEDIR = " out "/include/SOIL"))
                          (("LIBDIR = /usr/lib")
                           (string-append "LIBDIR = " out "/lib"))
                          ;; Remove these flags from 'install' commands.
                          (("-o root -g root") ""))))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("mesa" ,mesa)))
    (home-page "http://www.lonesock.net/soil.html")
    (synopsis "OpenGL texture loading library")
    (description
     "SOIL is a tiny C library used primarily for uploading textures into
OpenGL.")
    (license license:public-domain)))

(define-public glfw
  (package
    (name "glfw")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/glfw/glfw"
                                  "/releases/download/" version
                                  "/glfw-" version ".zip"))
              (sha256
               (base32
                "09kk5yc1zhss9add8ryqrngrr16hdmc94rszgng135bhw09mxmdp"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; no test target
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("unzip" ,unzip)))
    (propagated-inputs
     `(("mesa" ,mesa)                             ;included in public headers

       ;; These are in 'Requires.private' of 'glfw3.pc'.
       ("libx11" ,libx11)
       ("libxrandr" ,libxrandr)
       ("libxinerama" ,libxinerama)
       ("libxcursor" ,libxcursor)
       ("libxxf86vm" ,libxxf86vm)))
    (home-page "http://www.glfw.org")
    (synopsis "OpenGL application development library")
    (description
     "GLFW is a library for OpenGL, OpenGL ES and Vulkan development for
desktop computers.  It provides a simple API for creating windows, contexts
and surfaces, receiving input and events.")
    (license license:zlib)))

(define-public nanovg-for-extempore
  (package
    (name "nanovg-for-extempore")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/extemporelang/nanovg/"
                                  "archive/"  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ivs1sagq19xiw8jxd9f8w2b39svi0n9hrbmdvckwvqg95r8701g"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no tests included
    (inputs
     `(("mesa" ,mesa)))
    ;; Extempore refuses to build on architectures other than x86_64
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/extemporelang/nanovg")
    (synopsis "2D vector drawing library on top of OpenGL")
    (description "NanoVG is small antialiased vector graphics rendering
library for OpenGL.  It has lean API modeled after HTML5 canvas API.  It is
aimed to be a practical and fun toolset for building scalable user interfaces
and visualizations.")
    (license license:zlib)))

(define-public gl2ps
  (package
    (name "gl2ps")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://geuz.org/gl2ps/src/gl2ps-"
             version ".tgz"))
       (sha256
        (base32
         "1qpidkz8x3bxqf69hlhyz1m0jmfi9kq24fxsp7rq6wfqzinmxjq3"))))
    (build-system cmake-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("mesa" ,mesa)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f))  ;; no tests
    (home-page "http://www.geuz.org/gl2ps/")
    (synopsis "OpenGL to PostScript printing library")
    (description "GL2PS is a C library providing high quality vector
output for any OpenGL application.  GL2PS uses sorting algorithms
capable of handling intersecting and stretched polygons, as well as
non-manifold objects.  GL2PS provides many features including advanced
smooth shading and text rendering, culling of invisible primitives and
mixed vector/bitmap output.")
    ;; GL2PS is dual-licenced and can be used under the terms of either.
    (license (list license:lgpl2.0+
                   (license:fsf-free "http://www.geuz.org/gl2ps/COPYING.GL2PS"
                                     "GPL-incompatible copyleft license")))))

(define-public virtualgl
  (package
    (name "virtualgl")
    (version "2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VirtualGL/virtualgl.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0di2igj2bhwb153fndgxks7y57pyhp0gj31n47j93gb7lxc9qcck"))))
    (arguments
     `(#:tests? #f                      ; no tests are available
       #:configure-flags (list
                          (string-append "-DCMAKE_INSTALL_LIBDIR="
                                         (assoc-ref %outputs "out") "/lib")
                          "-DVGL_USESSL=1"))) ; use OpenSSL
    (build-system cmake-build-system)
    (inputs `(("glu" ,glu)
              ("libjpeg-turbo" ,libjpeg-turbo)
              ("libxtst" ,libxtst)
              ("mesa" ,mesa)
              ("openssl" ,openssl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.virtualgl.org")
    (synopsis "Redirects 3D commands from an OpenGL application onto a 3D
graphics card")
    (description "VirtualGL redirects the 3D rendering commands from OpenGL
applications to 3D accelerator hardware in a dedicated server and displays the
rendered output interactively to a thin client located elsewhere on the
network.")
    (license license:wxwindows3.1+)))
