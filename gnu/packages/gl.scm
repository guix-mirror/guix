;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages guile))

(define-public glu
  (package
    (name "glu")
    (version "9.0.0")
    (source (origin
	     (method url-fetch)
	     (uri (string-append "ftp://ftp.freedesktop.org/pub/mesa/glu/glu-"
				  version ".tar.gz"))
	     (sha256
	      (base32 "0r72yyhj09x3krn3kn629jqbwyq50ji8w5ri2pn6zwrk35m4g1s3"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)))
    (home-page "http://www.opengl.org/archives/resources/faq/technical/glu.htm")
    (synopsis "Mesa OpenGL Utility library")
    (description
     "GLU, or OpenGL Utility Library provides some higher-level functionality
not provided by just OpenGL itself. Some of GLU's Features
include: Scaling of 2D images and creation of mipmap pyramids,
Transformation of object coordinates into device coordinates and
vice versa, Support for NURBS surfaces, Support for tessellation
of concave or bow tie polygonal primitives, Specialty transformation
matrices for creating perspective and orthographic projections,
positioning a camera, and selection/picking, Rendering of disk,
cylinder, and sphere primitives, Interpreting OpenGL error values
as ASCII text.")
    (license (l:x11-style "http://directory.fsf.org/wiki/License:SGIFreeBv2"))))

(define-public freeglut
  (package
    (name "freeglut")
    (version "2.8.1")
    (source (origin
	     (method url-fetch)
	     (uri (string-append "mirror://sourceforge/project/freeglut/freeglut/"
				  version "/freeglut-" version ".tar.gz"))
	     (sha256
	      (base32 "16lrxxxd9ps9l69y3zsw6iy0drwjsp6m26d1937xj71alqk6dr6x"))))
    (build-system gnu-build-system)
    (inputs `(("mesa" ,mesa)
	      ("libx11" ,libx11)
	      ("libxi" ,libxi)
	      ("libxrandr" ,libxrandr)
	      ("libxxf86vm" ,libxxf86vm)
	      ("inputproto" ,inputproto)
	      ("xinput" ,xinput)
	      ("glu" ,glu)))
    (home-page "http://freeglut.sourceforge.net/")
    (synopsis "Alternative to the OpenGL Utility Toolkit (GLUT)")
    (description
     "Freeglut is a completely Free/OpenSourced alternative to
the OpenGL Utility Toolkit (GLUT) library. GLUT was originally
written by Mark Kilgard to support the sample programs in the
second edition OpenGL 'RedBook'. Since then, GLUT has been used
in a wide variety of practical applications because it is simple,
widely available and highly portable.

GLUT (and hence freeglut) allows the user to create and manage windows
containing OpenGL contexts on a wide range of platforms and also read
the mouse, keyboard and joystick functions. freeglut is released under
the X-Consortium license.")
    (license l:x11)))

(define-public ftgl
  (package
    (name "ftgl")
    (version "2.1.3-rc5")
    (source (origin
	     (method url-fetch)
	     (uri (string-append "mirror://sourceforge/project/ftgl/FTGL%20Source/2.1.3~rc5/ftgl-"
				  version ".tar.gz"))
	     (sha256
	      (base32 "0nsn4s6vnv5xcgxcw6q031amvh2zfj2smy1r5mbnjj2548hxcn2l"))))
    (build-system gnu-build-system)
    (inputs `(("freetype" ,freetype)
	      ("libx11" ,libx11)
	      ("mesa" ,mesa)
	      ("glu" ,glu)))
    (home-page "http://ftgl.sourceforge.net")
    (synopsis "Font rendering library for OpenGL applications")
    (description
     "FTGL is a font rendering library for OpenGL applications. Supported
rendering modes are: Bitmaps, Anti-aliased pixmaps, Texture maps, Outlines,
Polygon meshes, and Extruded polygon meshes")
    (license l:x11)))

(define-public mesa
  (package
    (name "mesa")
    ;; In newer versions (9.0.5, 9.1 and 9.2 tested), "make" results in an
    ;; infinite configure loop, see
    ;; https://bugs.freedesktop.org/show_bug.cgi?id=58812
    (version "8.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "ftp://ftp.freedesktop.org/pub/mesa/older-versions/8.x/"
               version
               "/MesaLib-" version
               ".tar.bz2"))
        (sha256
          (base32
            "0pjs8x51c0i6mawgd4w03lxpyx5fnx7rc8plr8jfsscf9yiqs6si"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("glproto" ,glproto)
        ("libdrm" ,libdrm-2.4.33)
        ("libx11" ,libx11)
        ("libxdamage" ,libxdamage)
        ("libxxf86vm" ,libxxf86vm)))
    (inputs
      `(("dri2proto" ,dri2proto)
        ("expat" ,expat)
        ("libxfixes" ,libxfixes)
        ("libxml2" ,libxml2)
        ("makedepend" ,makedepend)))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ("flex" ,flex)
        ("bison" ,bison)
        ("python" ,python-2))) ; incompatible with Python 3 (print syntax)
    (arguments
      `(#:configure-flags
         `("--with-gallium-drivers=r600,svga,swrast") ; drop r300 from the default list as it requires llvm
        #:phases
         (alist-cons-after
          'unpack 'remove-symlink
          (lambda _
            ;; remove dangling symlink to /usr/include/wine/windows
            (delete-file "src/gallium/state_trackers/d3d1x/w32api"))
         %standard-phases)))
    (home-page "http://mesa3d.org/")
    (synopsis "Mesa, an OpenGL implementation")
    (description "Mesa is a free implementation of the OpenGL specification -
a system for rendering interactive 3D graphics. A variety of device drivers
allows Mesa to be used in many different environments ranging from software
emulation to complete hardware acceleration for modern GPUs.")
    (license l:x11)))

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
    (inputs `(("guile" ,guile-2.0)
              ("mesa" ,mesa)
              ("freeglut" ,freeglut)))
    (arguments
     '(#:phases (alist-cons-before
                 'build 'patch-dynamic-link
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
                   (dynamic-link-substitute "glu/runtime.scm" "GLU" "mesa")
                   (dynamic-link-substitute "glut/runtime.scm" "glut"
                                            "freeglut"))
                 %standard-phases)))
    (home-page "http://gnu.org/s/guile-opengl")
    (synopsis "Guile binding for the OpenGL graphics API")
    (description
     "Guile-OpenGL is a library for Guile that provides bindings to the
OpenGL graphics API.")
    (license l:lgpl3+)))
