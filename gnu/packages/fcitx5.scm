;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
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

(define-module (gnu packages fcitx5)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public xcb-imdkit
  (package
    (name "xcb-imdkit")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.fcitx-im.org/fcitx5/xcb-imdkit/xcb-imdkit-"
             version ".tar.xz"))
       (sha256
        (base32 "1qgbbp8y8ci7haz99vgbrgpjsbrwwyjianyhdvxcirnbm5bybvmz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled uthash.
           (delete-file-recursively "uthash")
           #t))))
    (build-system cmake-build-system)
    (inputs
     `(("uthash" ,uthash)
       ("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/fcitx/xcb-imdkit")
    (synopsis "Input method development support for XCB")
    (description "Xcb-imdkit is an implementation of xim protocol in XCB,
comparing with the implementation of IMDkit with Xlib, and xim inside Xlib, it
has less memory foot print, better performance, and safer on malformed
client.")
    (license license:lgpl2.1)))

(define-public fcitx5
  (package
    (name "fcitx5")
    (version "5.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.fcitx-im.org/fcitx5/fcitx5/fcitx5-"
             version "_dict.tar.xz"))
       (sha256
        (base32 "06zkb33m2rnhg385iy79n3r4svz5jbav74di61xqa3lhbv7534s3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCLDR_DIR="
                            (assoc-ref %build-inputs "unicode-cldr-common")
                            "/share/unicode/cldr"))))
    (inputs
     `(("cairo" ,cairo)
       ("cairo-xcb" ,cairo-xcb)
       ("dbus" ,dbus)
       ("enchant" ,enchant)
       ("expat" ,expat)
       ("fmt" ,fmt)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("iso-codes" ,iso-codes)
       ("json-c" ,json-c)
       ("libevent" ,libevent)
       ("libpthread-stubs" ,libpthread-stubs)
       ("libuuid" ,util-linux "lib")
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxfixes" ,libxfixes)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxkbfile" ,libxkbfile)
       ("pango" ,pango)
       ("unicode-cldr-common" ,unicode-cldr-common)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("xcb-imdkit" ,xcb-imdkit)
       ("xcb-util" ,xcb-util)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-wm" ,xcb-util-wm)
       ("xkeyboard-config" ,xkeyboard-config)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (native-search-paths
     (list (search-path-specification
            (variable "FCITX_ADDON_DIRS")
            (files '("lib/fcitx5")))))
    (home-page "https://github.com/fcitx/fcitx5")
    (synopsis "Input method framework")
    (description "Fcitx 5 is a generic input method framework.")
    (license license:lgpl2.1+)))

(define-public fcitx5-lua
  (package
    (name "fcitx5-lua")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.fcitx-im.org/fcitx5/fcitx5-lua/fcitx5-lua-"
             version ".tar.xz"))
       (sha256
        (base32 "177mj56j8yrl79hvk7bbrifvm137np23pwalv83ibgk4l51z92hf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append "-DFEM_INCLUDE_INSTALL_DIR=" %output "/include")
        (string-append "-DFEM_LIB_INSTALL_DIR=" %output "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-install-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
              (lambda (x)
                (format #t "patch-install-prefix: Fixing install prefix in ~a~%"
                        x)
                (substitute* x
                  (("\\$\\{FCITX_INSTALL_PKGDATADIR\\}")
                   (string-append (assoc-ref outputs "out")
                                  "/share/fcitx5"))))
              '("src/addonloader/CMakeLists.txt"
                "src/imeapi/CMakeLists.txt")))))))
    (inputs
     `(("fcitx5" ,fcitx5)
       ("lua" ,lua)
       ("gettext" ,gettext-minimal)
       ("libpthread-stubs" ,libpthread-stubs)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (home-page "https://github.com/fcitx/fcitx5-lua")
    (synopsis "Lua support for Fcitx 5")
    (description "Fcitx5-lua allows writing Fcitx5 extension in Lua.")
    (license license:lgpl2.1+)))

(define-public libime
  (package
    (name "libime")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.fcitx-im.org/fcitx5/libime/libime-"
                           version "_dict.tar.xz"))
       (sha256
        (base32 "006pncby7p6h3rnicckzjwi6jzsrqiqbj6p9bpic80lanlllgw31"))))
    (build-system cmake-build-system)
    (inputs
     `(("fcitx5" ,fcitx5)
       ("boost" ,boost)))
    (native-inputs
     `(("gcc" ,gcc-9)                  ;for #include <filesystem> and ld support
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("python" ,python)))             ;needed to run test
    (home-page "https://github.com/fcitx/libime")
    (synopsis "Library for implementing generic input method")
    (description "Libime is a library for implmenting various input methods
editors.")
    (license license:lgpl2.1+)))

(define-public fcitx5-gtk
  (package
    (name "fcitx5-gtk")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.fcitx-im.org/fcitx5"
                           "/fcitx5-gtk/fcitx5-gtk-"
                           version ".tar.xz"))
       (sha256
        (base32 "0h53liraqc5nz4nyi3ixdfdw3zzkdcsiff7j25acc3gmaa5gyij7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;No test
       #:configure-flags
       (list (string-append "-DGOBJECT_INTROSPECTION_GIRDIR="
                            %output "/share/gir-1.0")
             (string-append "-DGOBJECT_INTROSPECTION_TYPELIBDIR="
                            %output "/lib/girepository-1.0"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-install-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gtk2 (assoc-ref outputs "gtk2")))
               ;; Install GTK+ 2 input method module to its own output.
               (substitute* "gtk2/CMakeLists.txt"
                 (("\\$\\{CMAKE_INSTALL_LIBDIR\\}")
                  (string-append gtk2 "/lib")))))))))
    (inputs
     `(("fcitx5" ,fcitx5)
       ("libxkbcommon" ,libxkbcommon)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk2" ,gtk+-2)
       ("gtk3" ,gtk+)
       ("glib" ,glib)
       ("libx11" ,libx11)
       ("gettext" ,gettext-minimal)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")))           ;for glib-genmarshal
    ;; TODO: Add "lib" output to reduce the closure size of "gtk2".
    (outputs '("out" "gtk2"))
    (home-page "https://github.com/fcitx/fcitx5-gtk")
    (synopsis "Glib based D-Bus client and GTK IM module for Fcitx 5")
    (description "Fcitx5-gtk provides a Glib based D-Bus client and IM module
for GTK+2/GTK+3 application.")
    (license license:lgpl2.1+)))

(define-public fcitx5-qt
  (package
    (name "fcitx5-qt")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.fcitx-im.org/fcitx5"
                           "/fcitx5-qt/fcitx5-qt-"
                           version ".tar.xz"))
       (sha256
        (base32 "0ilhb4yw9k3m1c4fidnv3nd5dgm9xxds11dgdys6gswjjnmcgqqm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCMAKE_INSTALL_QT5PLUGINDIR="
                            %output "/lib/qt5/plugins")
             "-DENABLE_QT4=Off")))
    (inputs
     `(("fcitx5" ,fcitx5)
       ("libxcb" ,libxcb)
       ("libxkbcommon" ,libxkbcommon)
       ("qtbase" ,qtbase)
       ("gettext" ,gettext-minimal)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (home-page "https://github.com/fcitx/fcitx5-qt")
    (synopsis "Qt library and IM module for Fcitx 5")
    (description "Fcitx5-qt provides Qt library for development and IM module
for Qt based application.")
    (license (list license:lgpl2.1+
                   ;; Files under qt4(Fcitx5Qt4DBusAddons), qt5/dbusaddons
                   ;; and qt5/platforminputcontext.
                   license:bsd-3))))
