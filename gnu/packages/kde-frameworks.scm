;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages kde-frameworks)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define kde-frameworks-version "5.19.0")

(define-public extra-cmake-modules
  (package
    (name "extra-cmake-modules")
    (version kde-frameworks-version)
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://download.kde.org/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1dl3hhbara7iswb5wsc5dp17ar3ljw5f0nrncl8vry9smaz2zl63"))))
    ;; The package looks for Qt5LinguistTools provided by Qt, but apparently
    ;; compiles without it; it might be needed for building the
    ;; documentation, which requires the additional Sphinx package.
    ;; To save space, we do not add these inputs.
    (build-system cmake-build-system)
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "CMake module files for common software used by KDE")
    (description "The Extra CMake Modules package, or ECM, adds to the
modules provided by CMake to find common software.  In addition, it provides
common build settings used in software produced by the KDE community.")
    (license license:bsd-3)))

(define-public kwindowsystem
  (package
    (name "kwindowsystem")
    (version kde-frameworks-version)
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://download.kde.org/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "115xs34r74j9zcsw69glnh8w59iyh764n3gniawwrk23c6yb8fch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server))) ; for the tests
    (inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("libxrender" ,libxrender)
       ("qt" ,qt)
       ("xcb-utils-keysyms" ,xcb-util-keysyms)))
    (arguments
     `(#:tests? #f)) ; FIXME: The first seven tests fail with "Exception".
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE access to the windowing system")
    (description "KWindowSystem provides information about and allows
interaction with the windowing system.  It provides a high level API, which
is windowing system independent and has platform specific
implementations.  This API is inspired by X11 and thus not all functionality
is available on all windowing systems.

In addition to the high level API, this framework also provides several
lower level classes for interaction with the X Windowing System.")
    ;; Some source files mention lgpl2.0+, but the included license is
    ;; the lgpl2.1. Some source files are under non-copyleft licenses.
    (license license:lgpl2.1+)))

(define-public oxygen-icons
  (package
    (name "oxygen-icons")
    (version kde-frameworks-version)
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://download.kde.org/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "5-"version ".tar.xz"))
        (sha256
         (base32
          "09vfwcyidj3bl0qr4sq78bkc69zp9x8dwp8bsay5y05q8591dkg0"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qt" ,qt)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen provides the standard icon theme for the KDE desktop.")
    (description "Oxygen icon theme for the KDE desktop")
    (license license:lgpl3+)))
