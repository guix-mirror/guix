;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
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

(define-module (gnu packages mate)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python))

(define-public mate-icon-theme
  (package
    (name "mate-icon-theme")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zldw22p1i76iss8car39pmfagpfxxlfk1fdhvr4x5r6gf36gv7d"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gtk+" ,gtk+)
       ("icon-naming-utils" ,icon-naming-utils)))
    (home-page "http://mate-desktop.org/")
    (synopsis "The MATE desktop environment icon theme")
    (description
     "This package contains the default icon theme used by the MATE desktop.")
    (license license:lgpl3+)))

(define-public mate-themes
  (package
    (name "mate-themes")
    (version "3.22.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pub.mate-desktop.org/releases/themes/"
                                  (version-major+minor version) "/mate-themes-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "03ficjfxa4qpx4vcshhk2zxryivckxpw7wcjgbn8xqnjk3lgzjcb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gtk+" ,gtk+)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gtk-engines" ,gtk-engines)
       ("murrine" ,murrine)))
    (home-page "http://mate-desktop.org/")
    (synopsis
     "Official themes for the MATE desktop")
    (description
     "This package includes the standard themes for the MATE desktop, for
example Menta, TraditionalOk, GreenLaguna or BlackMate.  This package has
themes for both gtk+-2 and gtk+-3.")
    (license (list license:lgpl2.1+ license:cc-by-sa3.0 license:gpl3+
                   license:gpl2+))))

(define-public mate-desktop
  (package
    (name "mate-desktop")
    (version "1.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1pzncfgrzp2mp9407ivk1113hkadpf110blr058h31jkwsk8syjq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-gtk=3.0"
                           "--enable-mpaste")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("yelp-tools" ,yelp-tools)))
       ;;("gtk-doc" ,gtk-doc))) ; add back in when gtk-doc builds
    (inputs
     `(("gtk+" ,gtk+)
       ("libxrandr" ,libxrandr)
       ("python2" ,python-2)
       ("startup-notification" ,startup-notification)))
    (propagated-inputs
     `(("dconf" ,dconf))) ; mate-desktop-2.0.pc
    (home-page "http://mate-desktop.org/")
    (synopsis "Library with common API for various MATE modules")
    (description
     "This package contains a public API shared by several applications on the
desktop and the mate-about program.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.1+))))

(define-public libmateweather
  (package
    (name "libmateweather")
    (version "1.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0w1b8b1ckmkbvwnqi9yh2lwbskzhz99s5yxdkar5xiqylnjrwmm3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--with-gtk=3.0"
         ,(string-append "--with-zoneinfo-dir="
                         (assoc-ref %build-inputs "tzdata")
                         "/share/zoneinfo"))
       #:phases
       (modify-phases %standard-phases
         (add-before
          'check 'pre-check
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "data/check-timezones.sh"
              (("/usr/share/zoneinfo/zone.tab")
               (string-append (assoc-ref inputs "tzdata")
                              "/share/zoneinfo/zone.tab")))
            #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("dconf" ,dconf)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gtk+" ,gtk+)
       ("tzdata" ,tzdata)))
    (propagated-inputs
      ;; both of these are requires.private in mateweather.pc
     `(("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (home-page "http://mate-desktop.org/")
    (synopsis "MATE library for weather information from the Internet")
    (description
     "This library provides access to weather information from the internet for
the MATE desktop environment.")
    (license license:lgpl2.1+)))

(define-public mate-menus
  (package
    (name "mate-menus")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0crw07iszwsqk54y8znfqdgfz76rjdz8992v4q9kpwwlrl11xmc5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'fix-introspection-install-dir
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* '("configure")
                (("`\\$PKG_CONFIG --variable=girdir gobject-introspection-1.0`")
                 (string-append "\"" out "/share/gir-1.0/\""))
                (("\\$\\(\\$PKG_CONFIG --variable=typelibdir gobject-introspection-1.0\\)")
                 (string-append out "/lib/girepository-1.0/")))
              #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("glib" ,glib)
       ("python" ,python-2)))
    (home-page "http://mate-desktop.org/")
    (synopsis "Freedesktop menu specification implementation for MATE")
    (description
     "The package contains an implementation of the freedesktop menu
specification, the MATE menu layout configuration files, .directory files and
assorted menu related utility programs.")
    (license (list license:gpl2+ license:lgpl2.0+))))
