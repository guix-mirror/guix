;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages freedesktop)
  #:use-module ((guix licenses) #:select (expat x11))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public xdg-utils
  (package
    (name "xdg-utils")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
          (uri (string-append
                 "http://portland.freedesktop.org/download/xdg-utils-"
                 version ".tgz"))
          (sha256
            (base32
             "1b019d3r1379b60p33d6z44kx589xjgga62ijz9vha95dg8vgbi1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "http://portland.freedesktop.org/")
    (synopsis "Freedesktop.org scripts for desktop integration")
    (description "The xdg-utils package is a set of simple scripts that
provide basic desktop integration functions in the framework of the
freedesktop.org project.")
    (license expat)))

(define-public libinput
  (package
    (name "libinput")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://freedesktop.org/software/libinput/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "06n6ih2bfr957rprsgjxhi6f7m96wmf4kgac8y0ispsjvrzszv3c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libudev" ,eudev))) ; required by libinput.pc
    (inputs
     `(("libevdev" ,libevdev)
       ("mtdev" ,mtdev)))
    (home-page "http://www.freedesktop.org/wiki/Software/libinput/")
    (synopsis "Input devices handling library")
    (description
     "Libinput is a library to handle input devices for display servers and
other applications that need to directly deal with input devices.")
    (license x11)))
