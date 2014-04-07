;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
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
  #:use-module (guix licenses)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public fltk
  (package
    (name "fltk")
    (version "1.3.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://fltk.org/pub/fltk/" version "/fltk-" version "-source.tar.gz"))
      (sha256
       (base32
        "1974brlk723095vf8z72kazq1cbqr9a51kq6b0xda6zkjkgl8q0p"))))
   (build-system gnu-build-system)
    (inputs
      `(("libx11" ,libx11)
        ("mesa" ,mesa)))
    (arguments
     `(#:tests? #f                      ;TODO: compile programs in "test" dir
       #:configure-flags '("--enable-shared")
       #:phases
       (alist-cons-before
        'configure 'patch-makeinclude
        (lambda _
          (substitute* "makeinclude.in"
            (("/bin/sh") (which "sh"))))
        %standard-phases)))
    (home-page "https://www.fltk.org")
    (synopsis "3D C++ GUI library")
    (description "FLTK is a C++ GUI toolkit providing modern GUI functionality without the
bloat. It supports 3D graphics via OpenGL and its built-in GLUT emulation.
FLTK is designed to be small and modular enough to be statically linked, but
works fine as a shared library. FLTK also includes an excellent UI builder
called FLUID that can be used to create applications in minutes.")
    (license lgpl2.0))) ; plus certain additional permissions
