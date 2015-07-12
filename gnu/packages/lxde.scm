;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
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

(define-module (gnu packages lxde)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public libfm-extra
  (package
    (name "libfm-extra")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/pcmanfm/"
                                  "PCManFM%20%2B%20Libfm%20%28tarball%20"
                                  "release%29/LibFM/libfm-" version ".tar.xz"))
              (sha256
               (base32
                "1ygvw52262r3jp1f45m9cdpx5xgvd4rkyfszslfqvg2c99ig34n6"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--with-extra-only")))
    (inputs `(("glib" ,glib)))
    (native-inputs `(("intltool"   ,intltool)
                     ("libtool"    ,libtool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "File management support (extra library)")
    (description "LibFM provides file management functions built on top of
Glib/GIO giving a higher-level API.  This package contains standalone library
which extends libfm.")
    (home-page "http://lxde.org")
    (license license:gpl2+)))

(define-public lxappearance
  (package
    (name "lxappearance")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/lxde/"
                                  "LXAppearance/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1phnv1b2jdj2vlibjyc9z01izcf3k5zxj8glsaf0i3vh77zqmqq9"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE GTK+ theme switcher")
    (description "LXAppearance is a desktop-independent GTK+ theme switcher
able to change themes, icons, and fonts used by GTK+ applications.")
    (home-page "http://lxde.org")
    (license license:gpl2+)))

(define-public menu-cache
  (package
    (name "menu-cache")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/lxde/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1bws84fiwk3anp30hcr0lw1xw5cgp44x5ik2yv2ijcgxpcvz8zgz"))))
    (build-system gnu-build-system)
    (inputs `(("glib"  ,glib)
              ("libfm" ,libfm-extra)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "LXDE implementation of the freedesktop menu's cache")
    (description "Menu-cache is a library creating and utilizing caches to
speed up the access to freedesktop.org defined application menus.")
    (home-page "http://lxde.org")
    (license license:lgpl2.1+)))

;;; lxde.scm ends here
