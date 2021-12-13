;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages tv)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public tvtime
  (package
    (name "tvtime")
    (version "1.0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://linuxtv.org/downloads/tvtime/tvtime-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1367rl3n6qxwf30lqyz234zpb43s9xjhig3hrvbg7cbqcl8g4fs0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-glibc-compatability
           (lambda _
             (substitute* "src/get_media_devices.c"
               (("<sys/stat.h>")
                "<sys/stat.h>\n#include <sys/sysmacros.h>"))
             #t)))))
    (inputs
     (list alsa-lib
           libx11
           libxext
           libxt
           libxtst
           libxinerama
           libxv
           libxxf86vm
           libpng
           libxml2
           freetype
           zlib))
    (native-inputs
     (list pkg-config))
    (home-page "http://tvtime.sourceforge.net")
    (synopsis "Television viewer")
    (description
     "Tvtime processes the input from your video capture card and
displays it on a monitor.  It focuses on a high visual quality.")
    (license license:gpl2+)))
