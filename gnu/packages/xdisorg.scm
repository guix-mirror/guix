;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages xdisorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))



;; packages outside the x.org system proper

(define-public xeyes
  (package
    (name "xeyes")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://xeyes.sourcearchive.com/downloads/1.0.1/xeyes_"
               version
               ".orig.tar.gz"))
        (sha256
          (base32
            "04c3md570j67g55h3bix1qbngcslnq91skli51k3g1avki88zkm9"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxext" ,libxext)
        ("libxmu" ,libxmu)
        ("libxt" ,libxt)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://xeyes.sourcearchive.com/")
    (synopsis "Follow-the-mouse X demo")
    (description "Xeyes is a demo program for x.org.  It shows eyes
following the mouse.")
    (license license:x11)))


(define-public pixman
  (package
    (name "pixman")
    (version "0.32.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://cairographics.org/releases/pixman-"
               version
               ".tar.gz"))
        (sha256
          (base32
           "113ycngcssbrps217dyajq96hm9xghsfch82h14yffla1r1fviw0"))))
    (build-system gnu-build-system)
    (inputs
      `(("libpng" ,libpng)
        ("zlib" ,zlib)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "http://www.pixman.org/")
    (synopsis "Low-level pixel manipulation library")
    (description "Pixman is a low-level software library for pixel
manipulation, providing features such as image compositing and trapezoid
rasterisation.")
    (license license:x11)))


(define-public libdrm
  (package
    (name "libdrm")
    (version "2.4.46")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://dri.freedesktop.org/libdrm/libdrm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1wah4qmrrcv0gnx65lhrlxb6gprxch92wy8lhxv6102fml6k5krk"))))
    (build-system gnu-build-system)
    (inputs
      `(("libpciaccess" ,libpciaccess)
        ("libpthread-stubs" ,libpthread-stubs)))
    (native-inputs
       `(("pkg-config" ,pkg-config)))
    (home-page "http://dri.freedesktop.org/wiki/")
    (synopsis "Direct rendering userspace library")
    (description "The Direct Rendering Infrastructure, also known as the DRI,
is a framework for allowing direct access to graphics hardware under the
X Window System in a safe and efficient manner.  It includes changes to the
X server, to several client libraries, and to the kernel (DRM, Direct
Rendering Manager).  The most important use for the DRI is to create fast
OpenGL implementations providing hardware acceleration for Mesa.
Several 3D accelerated drivers have been written to the DRI specification,
including drivers for chipsets produced by 3DFX, AMD (formerly ATI), Intel
and Matrox.")
    (license license:x11)))


;; old version, required by old mesa, see
;; http://www.mail-archive.com/nouveau@lists.freedesktop.org/msg10098.html
(define-public libdrm-2.4.33
  (package (inherit libdrm)
    (version "2.4.33")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://dri.freedesktop.org/libdrm/libdrm-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1slgi61n4dlsfli47ql354fd1ppj7n40jd94wvnsdqx0mna9syrd"))))
    (arguments
      `(#:configure-flags
         ;; create libdrm_nouveau.so, needed by mesa, see
         ;; http://comments.gmane.org/gmane.linux.lfs.beyond.support/43261
         `("--enable-nouveau-experimental-api")))))


(define-public mtdev
  (package
    (name "mtdev")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://bitmath.org/code/mtdev/mtdev-"
               version ".tar.bz2"))
        (sha256
          (base32
            "159ndzwfpw0xr8mw4lhl47w9c2krshlfrscs7k6n186vknv2hk3d"))))
    (build-system gnu-build-system)
    (home-page "http://bitmath.org/code/mtdev/")
    (synopsis "Multitouch protocol translation library")
    (description "Mtdev is a stand-alone library which transforms all
variants of kernel MT events to the slotted type B protocol.  The events
put into mtdev may be from any MT device, specifically type A without
contact tracking, type A with contact tracking, or type B with contact
tracking.")
    (license license:x11)))
