;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
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
  #:use-module (gnu packages mono)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system waf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define-public glu
  (package
    (name "glu")
    (version "9.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.freedesktop.org/pub/mesa/glu/glu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xqhk9bn10nbvffw3r4p4rjslwz1l7gaycc0x2pqkr2irp7q9x7n"))))
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
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/freeglut/freeglut/"
                    version "/freeglut-" version ".tar.gz"))
              (sha256
               (base32
                "0s6sk49q8ijgbsrrryb7dzqx2fa744jhx1wck5cz5jia2010w06l"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ;no test target
       #:configure-flags '("-DFREEGLUT_BUILD_STATIC_LIBS=OFF")))
    (inputs `(("libx11" ,libx11)
              ("libxi" ,libxi)
              ("libxrandr" ,libxrandr)
              ("libxxf86vm" ,libxxf86vm)))
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
second edition OpenGL @code{RedBook}.  Since then, GLUT has been used
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
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))))

(define-public ftgl
  (package
    (name "ftgl")
    (version "2.4.0")
    (home-page "https://github.com/frankheckenbach/ftgl")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zjs1h9w30gajq9lndzvjsa26rsmr1081lb1fbpbj10yhcdcsc79"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    ;; The pkg-config file lists "freetype2" as Requires.private.
    (propagated-inputs `(("freetype" ,freetype)))
    (inputs `(("libx11" ,libx11)
              ("mesa" ,mesa)
              ("glu" ,glu)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
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
             (url "https://github.com/divVerent/s2tc")
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
    (version "20.2.4")
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
          "14m09bk7akj0k02lg8fhvvzbdsashlbdsgl2cw7wbqfj2mhdqwh5"))
        (patches
         (search-patches "mesa-skip-tests.patch"))))
    (build-system meson-build-system)
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
             ;; Note: update the 'clang' input of mesa-opencl when bumping this.
             `(("llvm" ,llvm-11)))
            (_
             `()))
        ("wayland" ,wayland)
        ("wayland-protocols" ,wayland-protocols)))
    (native-inputs
      `(("bison" ,bison)
        ("flex" ,flex)
        ("gettext" ,gettext-minimal)
        ,@(match (%current-system)
            ((or "x86_64-linux" "i686-linux")
             `(("glslang" ,glslang)))
            (_
             `()))
        ("pkg-config" ,pkg-config)
        ("python" ,python-wrapper)
        ("python-mako" ,python-mako)
        ("which" ,(@ (gnu packages base) which))))
    (outputs '("out" "bin"))
    (arguments
     `(#:configure-flags
       '(,@(match (%current-system)
             ((or "armhf-linux" "aarch64-linux")
              ;; TODO: Fix svga driver for aarch64 and armhf.
              '("-Dgallium-drivers=etnaviv,freedreno,kmsro,lima,nouveau,panfrost,r300,r600,swrast,tegra,v3d,vc4,virgl"))
             (_
              '("-Dgallium-drivers=iris,nouveau,r300,r600,radeonsi,svga,swrast,virgl")))
         ;; Enable various optional features.  TODO: opencl requires libclc,
         ;; omx requires libomxil-bellagio
         "-Dplatforms=x11,drm,surfaceless,wayland"
         "-Dglx=dri"        ;Thread Local Storage, improves performance
         ;; "-Dopencl=true"
         ;; "-Domx=true"
         "-Dosmesa=gallium"
         "-Dgallium-xa=true"

         ;; features required by wayland
         "-Dgles2=true"
         "-Dgbm=true"
         "-Dshared-glapi=true"

         ;; Enable Vulkan on i686-linux and x86-64-linux.
         ,@(match (%current-system)
             ((or "i686-linux" "x86_64-linux")
              '("-Dvulkan-drivers=intel,amd"))
             (_
              '("-Dvulkan-drivers=auto")))

         ;; Enable the Vulkan overlay layer on i686-linux and x86-64-linux.
         ,@(match (%current-system)
             ((or "x86_64-linux" "i686-linux")
              '("-Dvulkan-overlay-layer=true"))
             (_
              '()))

         ;; Also enable the tests.
         "-Dbuild-tests=true"

         ;; on non-intel systems, drop i915 and i965
         ;; from the default dri drivers
         ,@(match (%current-system)
             ((or "x86_64-linux" "i686-linux")
              '("-Ddri-drivers=i915,i965,nouveau,r200,r100"
                "-Dllvm=true"))         ; default is x86/x86_64 only
             (_
              '("-Ddri-drivers=nouveau,r200,r100"))))

       ;; XXX: 'debugoptimized' causes LTO link failures on some drivers.  The
       ;; documentation recommends using 'release' for performance anyway.
       #:build-type "release"

       #:modules ((ice-9 match)
                  (srfi srfi-1)
                  (guix build utils)
                  (guix build meson-build-system))
       #:phases
       (modify-phases %standard-phases
         ,@(if (string-prefix? "i686" (or (%current-target-system)
                                          (%current-system)))
               ;; Disable new test from Mesa 19 that fails on i686.  Upstream
               ;; report: <https://bugs.freedesktop.org/show_bug.cgi?id=110612>.
               `((add-after 'unpack 'disable-failing-test
                   (lambda _
                     (substitute* "src/util/tests/format/meson.build"
                       (("'u_format_test',") ""))
                     #t)))
               '())
         (add-before 'configure 'fix-dlopen-libnames
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Remain agnostic to .so.X.Y.Z versions while doing
               ;; the substitutions so we're future-safe.
               (substitute* "src/glx/meson.build"
                 (("-DGL_LIB_NAME=\"lib@0@\\.so\\.@1@\"")
                  (string-append "-DGL_LIB_NAME=\"" out
                                 "/lib/lib@0@.so.@1@\"")))
               (substitute* "src/gbm/backends/dri/gbm_dri.c"
                 (("\"libglapi\\.so")
                  (string-append "\"" out "/lib/libglapi.so")))
               (substitute* "src/gbm/main/backend.c"
                 ;; No need to patch the gbm_gallium_drm.so reference;
                 ;; it's never installed since Mesa removed its
                 ;; egl_gallium support.
                 (("\"gbm_dri\\.so")
                  (string-append "\"" out "/lib/dri/gbm_dri.so")))
               #t)))
         (add-after 'install 'split-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bin (assoc-ref outputs "bin")))
               ,@(match (%current-system)
                   ((or "i686-linux" "x86_64-linux")
                    ;; Install the Vulkan overlay control script to a separate
                    ;; output to prevent a reference on Python, saving ~70 MiB
                    ;; on the closure size.
                    '((copy-recursively (string-append out "/bin")
                                        (string-append bin "/bin"))
                      (delete-file-recursively (string-append out "/bin"))))
                   (_
                    ;; XXX: On architectures without the Vulkan overlay layer
                    ;; just create an empty file because outputs can not be
                    ;; added conditionally.
                    '((mkdir-p (string-append bin "/bin"))
                      (call-with-output-file (string-append bin "/bin/.empty")
                        (const #t)))))
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

(define-public mesa-opencl
  (package/inherit mesa
    (name "mesa-opencl")
    (arguments
     (substitute-keyword-arguments (package-arguments mesa)
       ((#:configure-flags flags)
        `(cons "-Dgallium-opencl=standalone" ,flags))))
    (inputs
     `(("libclc" ,libclc)
       ,@(package-inputs mesa)))
    (native-inputs
     `(("clang" ,clang-11)
       ,@(package-native-inputs mesa)))))

(define-public mesa-opencl-icd
  (package/inherit mesa-opencl
    (name "mesa-opencl-icd")
    (arguments
     (substitute-keyword-arguments (package-arguments mesa)
       ((#:configure-flags flags)
        `(cons "-Dgallium-opencl=icd"
               ,(delete "-Dgallium-opencl=standalone" flags)))))))

(define-public mesa-headers
  (package/inherit mesa
    (name "mesa-headers")
    (propagated-inputs '())
    (inputs '())
    (native-inputs '())
    (outputs '("out"))
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
    (home-page "https://mesa3d.org/")
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
                     (substitute* "gl/runtime.scm"
                       (("\\(dynamic-link\\)")
                        (string-append "(dynamic-link \""
                                       (assoc-ref inputs "mesa")
                                       "/lib/libGL.so" "\")")))
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

(define-public guile3.0-opengl
  (package
    (inherit guile-opengl)
    (name "guile3.0-opengl")
    (arguments
     (substitute-keyword-arguments (package-arguments guile-opengl)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'build-with-guile-3.0
             (lambda _
               (substitute* "configure"
                 (("_guile_versions_to_search=\"")
                  "_guile_versions_to_search=\"3.0 "))
               #t))))))
    (inputs
     `(("guile" ,guile-3.0)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("freeglut" ,freeglut)))))

(define-public libepoxy
  (package
    (name "libepoxy")
    (version "1.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/anholt/libepoxy/releases/download/"
                    version "/libepoxy-" version ".tar.xz"))
              (sha256
               (base32
                "1ll9fach4v30dsyd47s5ial4gaiwihzr2afb77vxxzzy3mlcrlhb"))))
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

(define-public libglvnd
  (package
    (name "libglvnd")
    (version "1.3.2")
    (home-page "https://gitlab.freedesktop.org/glvnd/libglvnd")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10x7fgb114r4gikdg6flszl3kwzcb9y5qa7sj9936mk0zxhjaylz"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dx11=enabled")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-glx-tests
                    (lambda _
                      ;; This package is meant to be used alongside Mesa.
                      ;; To avoid a circular dependency, disable tests that
                      ;; require a running Xorg server.
                      (substitute* "tests/meson.build"
                        (("if with_glx")
                         "if false"))
                      #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("xorgproto" ,xorgproto)))
    (synopsis "Vendor-neutral OpenGL dispatch library")
    (description
     "libglvnd is a vendor-neutral dispatch layer for arbitrating OpenGL
API calls between multiple vendors.  It allows multiple drivers from
different vendors to coexist on the same file system, and determines which
vendor to dispatch each API call to at runtime.

Both GLX and EGL are supported, in any combination with OpenGL and OpenGL ES.")
    ;; libglvnd is available under a custom X11-style license, and incorporates
    ;; code with various other licenses.  See README.md for details.
    (license (list (license:x11-style "file://README.md")
                   license:x11
                   license:expat))))

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
    (home-page "https://www.lonesock.net/soil.html")
    (synopsis "OpenGL texture loading library")
    (description
     "SOIL is a tiny C library used primarily for uploading textures into
OpenGL.")
    (license license:public-domain)))

(define-public glfw
  (package
    (name "glfw")
    (version "3.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/glfw/glfw"
                                  "/releases/download/" version
                                  "/glfw-" version ".zip"))
              (sha256
               (base32
                "1kcrpl4d6b6h23ib5j9q670d9w3knd07whgbanbmwwhbcqnc9lmv"))))
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
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxcursor" ,libxcursor)
       ("libxxf86vm" ,libxxf86vm)))
    (home-page "https://www.glfw.org")
    (synopsis "OpenGL application development library")
    (description
     "GLFW is a library for OpenGL, OpenGL ES and Vulkan development for
desktop computers.  It provides a simple API for creating windows, contexts
and surfaces, receiving input and events.")
    (license license:zlib)))

(define-public nanovg-for-extempore
  (let ((version "0.7.1")
        (revision "0")
        (commit "3c60175fcc2e5fe305b04355cdce35d499c80310"))
    (package
      (name "nanovg-for-extempore")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/extemporelang/nanovg")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ddn3d3mxqn8hj9967v3pss7lz1wn08pcdnqzc118g7yjkq7hxzy"))))
      (build-system cmake-build-system)
      (arguments `(#:tests? #f))        ; no tests included
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
      (license license:zlib))))

(define-public gl2ps
  (package
    (name "gl2ps")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://geuz.org/gl2ps/src/gl2ps-"
             version ".tgz"))
       (sha256
        (base32 "1sgzv547h7hrskb9qd0x5yp45kmhvibjwj2mfswv95lg070h074d"))))
    (build-system cmake-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("mesa" ,mesa)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f))                    ; no tests
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
    (version "2.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VirtualGL/virtualgl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yyc553xsb5n0rx7jp9p4wdbd7md07b3qrkf3ssyjavqqg908qg9"))))
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

(define-public mojoshader
  (let ((changeset "5887634ea695"))
    (package
      (name "mojoshader")
      (version (string-append "20190825" "-" changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://hg.icculus.org/icculus/mojoshader/")
               (changeset changeset)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ibl4z1696jiifv9j5drir7jm0b5px0vwkwckbi7cfd46p7p6wcy"))))
      (arguments
       ;; Tests only for COMPILER_SUPPORT=ON.
       `(#:tests? #f
         #:configure-flags '("-DBUILD_SHARED=ON"
                             "-DFLIP_VIEWPORT=ON"
                             "-DDEPTH_CLIPPING=ON")
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib"))
                      (header (string-append out "/include")))
                 (install-file "libmojoshader.so" lib)
                 (for-each (lambda (f)
                             (install-file f header))
                           (find-files "../source" "mojoshader.*\\.h$"))
                 (let ((profiles-header (string-append header "/profiles")))
                   (mkdir-p profiles-header)
                   (rename-file (string-append header "/mojoshader_profile.h")
                                (string-append profiles-header "/mojoshader_profile.h"))))
               #t)))))
      (build-system cmake-build-system)
      (home-page "https://www.icculus.org/mojoshader/")
      (synopsis "Work with Direct3D shaders on alternate 3D APIs")
      (description "MojoShader is a library to work with Direct3D shaders on
alternate 3D APIs and non-Windows platforms.  The primary motivation is moving
shaders to OpenGL languages on the fly.  The developer deals with \"profiles\"
that represent various target languages, such as GLSL or ARB_*_program.

This allows a developer to manage one set of shaders, presumably written in
Direct3D HLSL, and use them across multiple rendering backends.  This also
means that the developer only has to worry about one (offline) compiler to
manage program complexity, while MojoShader itself deals with the reduced
complexity of the bytecode at runtime.

MojoShader provides both a simple API to convert bytecode to various profiles,
and (optionally) basic glue to rendering APIs to abstract the management of
the shaders at runtime.")
      (license license:zlib))))

(define-public mojoshader-with-viewport-flip
  ;; Changeset c586d4590241 replaced glProgramViewportFlip with
  ;; glProgramViewportInfo.
  ;; https://hg.icculus.org/icculus/mojoshader/rev/c586d4590241
  (let ((changeset "2e37299b13d8"))
    (package
      (inherit mojoshader)
      (name "mojoshader-with-viewport-flip")
      (version (string-append "20190725" "-" changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://hg.icculus.org/icculus/mojoshader/")
               (changeset changeset)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ffws7cqbskxwc3hjsnnzq4r2bbf008kdr3b11pa3kr7dsi50y6i"))))
      (synopsis "Work with Direct3D shaders on alternate 3D APIs (with viewport flip)")
      (description "This is the last version of the mojoshader library with
the glProgramViewportFlip before it was replaced with glProgramViewportInfo.")
      (license license:zlib))))

(define-public mojoshader-cs
  (let ((commit "10d0dba21ff1cfe332eb7de328a2adce01286bd7"))
    (package
      (name "mojoshader-cs")
      (version (git-version "20191205" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/FNA-XNA/MojoShader")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "11mdhf3fmb9rsn2iv753gmb596j4dh5j2iipgw078vg0lj23rml7"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f  ; No tests.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda _
               (invoke "make" "-C" "csharp")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "csharp/bin/MojoShader-CS.dll" (string-append out "/lib"))
                 #t))))))
      (native-inputs
       `(("mono" ,mono)))
      (home-page "https://github.com/FNA-XNA/MojoShader")
      (synopsis "C# wrapper for MojoShader")
      (description
       "Mojoshader-CS provides C# bindings for the Mojoshader library.
The C# wrapper was written to be used for FNA's platform support.  However, this
is written in a way that can be used for any general C# application.")
      (license license:zlib))))

(define-public glmark2
  (package
    (name "glmark2")
    (version "2020.04")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/glmark2/glmark2")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ywpzp0imi3f8iyp7d1739576zx2nsr3db5hp2as4yhflfyq1as2"))
              (modules '((guix build utils)))
              ;; Fix Python 3 incompatibility.
              (snippet
               '(begin
                  (substitute* "wscript"
                    (("(sorted\\()FLAVORS\\.keys\\(\\)(.*)" _ beginning end)
                     (string-append beginning "list(FLAVORS)" end)))
                  #t))))
    (build-system waf-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:configure-flags
       (list (string-append "--with-flavors="
                            (string-join '("x11-gl" "x11-glesv2"
                                           "drm-gl" "drm-glesv2"
                                           "wayland-gl" "wayland-glesv2")
                                         ",")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((mesa (assoc-ref inputs "mesa")))
               (substitute* (find-files "src" "gl-state-.*\\.cpp$")
                 (("libGL.so") (string-append mesa "/lib/libGL.so"))
                 (("libEGL.so") (string-append mesa "/lib/libEGL.so"))
                 (("libGLESv2.so") (string-append mesa "/lib/libGLESv2.so")))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("eudev" ,eudev)
       ("libdrm" ,libdrm)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("mesa" ,mesa)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)))
    (home-page "https://github.com/glmark2/glmark2")
    (synopsis "OpenGL 2.0 and OpenGL ES 2.0 benchmark")
    (description
     "glmark2 is an OpenGL 2.0 and OpenGL ES 2.0 benchmark based on the
original glmark benchmark by Ben Smith.")
    (license license:gpl3+)))
