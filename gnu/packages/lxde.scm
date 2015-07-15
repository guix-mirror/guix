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
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public libfm
  (package
    (name "libfm")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/pcmanfm/"
                                  "PCManFM%20%2B%20Libfm%20%28tarball%20release"
                                  "%29/LibFM/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ygvw52262r3jp1f45m9cdpx5xgvd4rkyfszslfqvg2c99ig34n6"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)
              ("gtk+" ,gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("glib"       ,glib "bin") ; for gtester
                     ("libtool"    ,libtool)
                     ("menu-cache" ,menu-cache)
                     ("pkg-config" ,pkg-config)
                     ("vala"       ,vala)))
    (synopsis "File management support (core library)")
    (description "LibFM provides file management functions built on top of
Glib/GIO giving a higher-level API.")
    (home-page "http://lxde.org")
    (license license:gpl2+)))

(define-public libfm-extra
  (package (inherit libfm)
    (name "libfm-extra")
    (arguments '(#:configure-flags '("--with-extra-only")))
    (inputs `(("glib" ,glib)))
    (native-inputs `(("intltool"   ,intltool)
                     ("libtool"    ,libtool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "File management support (extra library)")
    (description "This package contains standalone library which extends the
libFM file management library.")))

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

(define-public lxrandr
  (package
    (name "lxrandr")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/lxde/LXRandR"
                                  "%20%28monitor%20config%20tool%29/LXRandR%20"
                                  (version-major+minor version) ".x/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0xkbqv66hisbxkvnf7y5kwqbhrq26f49wd7w6ylhnjlccpnylg8q"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE monitor configuration tool")
    (description "LXRandR is a very basic monitor configuration tool.  It
relies on the X11 resize-and-rotate (RandR) extension but doesn't aim to be a
full frontend of it.  LXRandR only gives you some easy and quick options which
are intuitive.  It's suitable for laptop users who frequently uses projectors
or external monitor.")
    (home-page "http://lxde.org")
    (license license:gpl2+)))

(define-public lxtask
  (package
    (name "lxtask")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/lxde/LXTask"
                                  "%20%28task%20manager%29/LXTask%20"
                                  (version-major+minor version) ".x/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ia3i430lpwgl2kch6sl1za8qf96wc4fkcv91yhdzgnzafcnm3gp"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE task manager")
    (description "LXTask is a lightweight task manager derived from Xfce task
manager with all dependencies on Xfce removed.  LXTask is based on the GTK+
toolkit.  It allows users to monitor and control of running processes.")
    (home-page "http://lxde.org")
    (license license:gpl2+)))

(define-public lxterminal
  (package
    (name "lxterminal")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/lxde/LXTerminal"
                                  "%20%28terminal%20emulator%29/LXTerminal%20"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1brb506vmnncih8nyvlrckrrn6msbsvz2vwbm7bsqwigcnchwjqp"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("vte"  ,vte/gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE terminal emulator")
    (description "LXTerminal is a VTE-based terminal emulator.  It supports
multiple tabs and has only minimal dependencies thus being completely
desktop-independent.  In order to reduce memory usage and increase the
performance, all instances of the terminal are sharing a single process.")
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

(define-public pcmanfm
  (package
    (name "pcmanfm")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/project/" name "/"
                                  "PCManFM%20%2B%20Libfm%20%28tarball%20release"
                                  "%29/PCManFM/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1033rw5jd7nlzbcdpx3bik7347kyh1sg1gkla424gq9vqqpxia6g"))))
    (build-system gnu-build-system)
    ;; (#:configure-flags '("--sysconfdir=/etc")) suggested in README.
    (inputs `(("gtk+"   ,gtk+-2)
              ;; TODO: add ("gvfs" ,gvfs).
              ("libfm"  ,libfm)
              ("libx11" ,libx11)))
    (native-inputs `(("intltool"   ,intltool)
                     ("libtool"    ,libtool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE file manager")
    (description "PCMan is a lightweight GTK+ based file manager, compliant
with freedesktop.org standard.")
    (home-page "http://lxde.org")
    (license license:gpl2+)))

;;; lxde.scm ends here
