;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages display-managers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public greenisland
  (package
    (name "greenisland")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/greenisland/greenisland"
                    "/releases/download/v" version "/"
                    "greenisland-" version ".tar.xz"))
              (sha256
               (base32
                "1c9rlq7fqrsd5nb37anjvnp9xspqjz1kc0fvydv5xdy3abg8mw40"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("elogind" ,elogind)
       ("eudev" ,eudev)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("libdrm" ,libdrm)
       ("libinput" ,libinput-minimal)
       ("libxcursor" ,libxcursor)
       ("libxkbcommon" ,libxkbcommon)
       ("libx11" ,libx11)
       ("mesa" ,mesa)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("xcb-util-cursor" ,xcb-util-cursor)))
    (arguments
     `(#:configure-flags
       (list (string-append "-DPLUGIN_INSTALL_DIR="
                            (assoc-ref %outputs "out") "/plugins")
             (string-append "-DQML_INSTALL_DIR="
                            (assoc-ref %outputs "out") "/qml"))
       #:modules ((guix build cmake-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build qt-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-udev-tests
           (lambda _ ; FIXME: Build env doesn't contain /dev/sda
             (substitute* "tests/auto/platform/tst_udev.cpp"
               (("QVERIFY") "// QVERIFY")
               (("QCOMPARE") "// QCOMPARE"))))
         (replace 'check
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (zero? (system* "dbus-launch" "ctest" "."))))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; Enable debug output
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             #t))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-qt-program out "greenisland")
               (wrap-qt-program out "greenisland-launcher")
               (wrap-qt-program out "greenisland-screencaster")
               (wrap-qt-program out "greenisland-wayland-scanner")
               #t))))))
    (synopsis "QtQuick Wayland compositor and shell for desktop and mobile")
    (description "Green Island provides a full blown Wayland compositor for
QtQuick as well as pluggable hardware abstraction, extensions, tools and a
Qt-style API for Wayland clients.")
    (home-page "http://hawaiios.org")
    ;; Choice of license at the user's opinion.
    (license (list license:gpl2 license:gpl3 license:lgpl2.1 license:lgpl3))))

(define-public sddm
  (package
    (name "sddm")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/sddm/sddm"
                    "/releases/download/v" version "/"
                    "sddm-" version ".tar.xz"))
              (sha256
               (base32
                "0y3pn8g2qj7q20zkmbasrfsj925lfzizk63sfrvzf84bc5c84d3y"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("glib" ,glib)
       ("greenisland" ,greenisland)
       ("libxcb" ,libxcb)
       ("libxkbcommon" ,libxkbcommon)
       ("linux-pam" ,linux-pam)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("shadow" ,shadow)
       ("wayland" ,wayland)))
    (arguments
     `(#:configure-flags
       (list
        ;; Currently doesn't do anything
        ;; Option added by enable wayland greeters PR
        "-DENABLE_WAYLAND=ON"
        "-DENABLE_PAM=ON"
        "-DCONFIG_FILE=/etc/sddm.conf"
        ;; Set path to /etc/login.defs
        ;; Alternatively use -DUID_MIN and -DUID_MAX
        (string-append "-DLOGIN_DEFS_PATH="
                       (assoc-ref %build-inputs "shadow")
                       "/etc/login.defs")
        (string-append "-DQT_IMPORTS_DIR="
                       (assoc-ref %outputs "out") "/qml")
        (string-append "-DCMAKE_INSTALL_SYSCONFDIR="
                       (assoc-ref %outputs "out") "/etc"))
       #:modules ((guix build cmake-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build qt-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-qt-program out "sddm")
               (wrap-qt-program out "sddm-greeter")
               #t))))))
    (synopsis "QML based X11 and Wayland display manager")
    (description "SDDM is a display manager for X11 and Wayland aiming to be
fast, simple and beautiful.  SDDM is themeable and puts no restrictions on the
user interface design.  It uses QtQuick which gives the designer the ability to
create smooth, animated user interfaces.")
    (home-page "https://github.com/sddm/sddm")
    ;; QML files are MIT licensed and images are CC BY 3.0.
    (license (list license:gpl2+ license:expat license:cc-by3.0))))

(define-public slim
  (package
    (name "slim")
    (version "1.3.6")
    (source (origin
	     (method url-fetch)
             ;; Used to be available from download.berlios.de.
	     (uri (string-append
                   "mirror://sourceforge/slim.berlios/slim-"
                   version ".tar.gz"))
	     (sha256
	      (base32 "1pqhk22jb4aja4hkrm7rjgbgzjyh7i4zswdgf5nw862l2znzxpi1"))
             (patches (search-patches "slim-config.patch"
                                      "slim-session.patch"
                                      "slim-sigusr1.patch"))))
    (build-system cmake-build-system)
    (inputs `(("linux-pam" ,linux-pam)
	      ("libpng" ,libpng)
	      ("libjpeg" ,libjpeg)
	      ("freeglut" ,freeglut)
	      ("libxrandr" ,libxrandr)
	      ("libxrender" ,libxrender)
	      ("freetype" ,freetype)
	      ("fontconfig" ,fontconfig)
              ("libx11" ,libx11)
	      ("libxft" ,libxft)
	      ("libxmu" ,libxmu)
	      ("xauth" ,xauth)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:phases (alist-cons-before
		 'configure 'set-new-etc-location
		 (lambda _
		   (substitute* "CMakeLists.txt"
		     (("/etc")
		      (string-append (assoc-ref %outputs "out") "/etc"))
                     (("install.*systemd.*")
                      ;; The build system's logic here is: if "Linux", then
                      ;; "systemd".  Strip that.
                      "")))
		 %standard-phases)
       #:configure-flags '("-DUSE_PAM=yes"
                           "-DUSE_CONSOLEKIT=no")
       #:tests? #f))

    ;; This used to be at <http://slim.berlios.de/>.
    (home-page "http://sourceforge.net/projects/slim.berlios/")
    (synopsis "Desktop-independent graphical login manager for X11")
    (description
     "SLiM is a Desktop-independent graphical login manager for X11, derived
from Login.app.  It aims to be light and simple, although completely
configurable through themes and an option file; is suitable for machines on
which remote login functionalities are not needed.

Features included: PNG and XFT support for alpha transparency and antialiased
fonts, External themes support, Configurable runtime options: X server --
login / shutdown / reboot commands, Single (GDM-like) or double (XDM-like)
input control, Can load predefined user at startup, Configurable welcome /
shutdown messages, Random theme selection.")
    (license license:gpl2)))
