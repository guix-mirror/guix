;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
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

(define-module (gnu packages fltk)
  #:use-module ((guix licenses) #:select (lgpl2.0 lgpl2.0+))
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk) ;for "cairo"
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system waf))

(define-public fltk
  (package
    (name "fltk")
    (version "1.3.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://fltk.org/pub/fltk/" version
                          "/fltk-" version "-source.tar.gz"))
      (sha256
       (base32
        "15qd7lkz5d5ynz70xhxhigpz3wns39v9xcf7ggkl0792syc8sfgq"))
      (patches (search-patches "fltk-shared-lib-defines.patch"
                               "fltk-xfont-on-demand.patch"))))
   (build-system gnu-build-system)
   (inputs
    `(("libjpeg" ,libjpeg-8)     ;jpeg_read_header argument error in libjpeg-9
      ("libpng" ,libpng)
      ("libx11" ,libx11)
      ("libxft" ,libxft)
      ("mesa" ,mesa)
      ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f                      ;TODO: compile programs in "test" dir
       #:configure-flags
       (list "--enable-shared"
             (string-append "DSOFLAGS=-Wl,-rpath=" %output "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-makeinclude
           (lambda _
             (substitute* "makeinclude.in"
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'install 'patch-config
           ;; Provide -L flags for image libraries when querying fltk-config to
           ;; avoid propagating inputs.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (use-modules (srfi srfi-26))
             (let* ((conf (string-append (assoc-ref outputs "out")
                                         "/bin/fltk-config"))
                    (jpeg (assoc-ref inputs "libjpeg"))
                    (png  (assoc-ref inputs "libpng"))
                    (zlib (assoc-ref inputs "zlib")))
               (substitute* conf
                 (("-ljpeg") (string-append "-L" jpeg "/lib -ljpeg"))
                 (("-lpng") (string-append "-L" png "/lib -lpng"))
                 (("-lz") (string-append "-L" zlib "/lib -lz"))))
             #t)))))
    (home-page "http://www.fltk.org")
    (synopsis "3D C++ GUI library")
    (description "FLTK is a C++ GUI toolkit providing modern GUI functionality
without the bloat.  It supports 3D graphics via OpenGL and its built-in GLUT
emulation.  FLTK is designed to be small and modular enough to be statically
linked, but works fine as a shared library.  FLTK also includes an excellent
UI builder called FLUID that can be used to create applications in minutes.")
    (license lgpl2.0))) ; plus certain additional permissions

(define-public ntk
  (package
    (name "ntk")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.tuxfamily.org/gitroot/non/fltk.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0ggrh6rihf676z1vfgpgcl6kpqwyh39ih0hskcgzklh7fphfik39"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:python ,python-2
       #:configure-flags '("--enable-gl")
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-ldflags
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "LDFLAGS"
                    (string-append "-Wl,-rpath="
                                   (assoc-ref outputs "out") "/lib")))))))
    (inputs
     `(("libjpeg" ,libjpeg)
       ("glu" ,glu)))
    ;; ntk.pc lists "x11" and "xft" in Requires.private, and "cairo" in
    ;; Requires.
    (propagated-inputs
     `(("cairo" ,cairo)
       ("libxft" ,libxft)
       ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://non.tuxfamily.org/ntk/")
    (synopsis "Fork of FLTK with graphics rendering via Cairo")
    (description "The Non Tool Kit (NTK) is a fork of the Fast Light ToolKit
library, adding improved graphics rendering via Cairo, a streamlined and
enhanced widget set, and other features designed to improve the appearance and
performance of the Non applications.")
    (license lgpl2.0+))) ; plus certain additional permissions
