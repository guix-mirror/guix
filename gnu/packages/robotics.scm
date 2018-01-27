;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages robotics)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl))

(define-public enki
  ;; Previous versions use Qt4 and are unsuitable for Aseba.
  (let ((commit "afd2d8e2f91c095f6745505ca1f32f31ea874200")
        (revision "0"))
    (package
      (name "enki")
      (version (git-version "2.0pre" revision commit))
      (home-page "https://github.com/enki-community/enki/")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "1d1901zzsfml97hb4mb3ah3ab1bk4kh7bn6m7xrj1rv0gk9wkhq7"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
      (native-inputs `(("pkg-config" ,pkg-config)))
      (inputs
       ;; Optionally, add Python + Boost for Python bindings.
       `(("sdl2" ,sdl2)))
      (propagated-inputs
       ;; 'Viewer.h' includes 'QGLWidget'.
       `(("qtbase" ,qtbase)                ;the viewer module needs Qt5 + MESA
         ("mesa" ,mesa)))
      (synopsis "Robot simulator")
      (description
       "Enki is a robot simulator written in C++.  It provides collision and
limited physics support for robots evolving on a flat surface.  On a
contemporary desktop computer, Enki is able to simulate groups of robots
hundred times faster than real-time.")

      ;; The 'LICENSE' file as well as source file headers says that
      ;; researchers using the software are "asked" to cite using a given
      ;; citation, but that sentence is written as not being part of the
      ;; license (fortunately).
      (license license:gpl2+))))
