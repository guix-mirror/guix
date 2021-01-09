;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 L  p R n  d n <guix@lprndn.info>
;;; Copyright © 2020 Fredrik Salomonsson <plattfot@gmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg))

(define-public sddm
  (package
    (name "sddm")
    (version "0.19.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/sddm/sddm"
                    "/releases/download/v" version "/"
                    "sddm-" version ".tar.xz"))
              (sha256
               (base32
                "0hcdysw8ibr66vk8i7v56l0v5ijvhlq67v4460mc2xf2910g2m72"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("elogind" ,elogind)
       ("glib" ,glib)
       ("libxcb" ,libxcb)
       ("libxkbcommon" ,libxkbcommon)
       ("linux-pam" ,linux-pam)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)

       ;; Some user-defined themes use QtQuick components internally.  Adding
       ;; QtQuick & co. here; they end up in QML2_IMPORT_PATH thanks to
       ;; 'wrap-qt-program'.
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtsvg" ,qtsvg)

       ("shadow" ,shadow)
       ("wayland" ,wayland)))
    (arguments
     `(#:configure-flags
       (list
        ;; This option currently does nothing, but will presumably be enabled
        ;; if/when <https://github.com/sddm/sddm/pull/616> is merged.
        "-DENABLE_WAYLAND=ON"
        "-DENABLE_PAM=ON"
        ;; Both flags are required for elogind support.
        "-DNO_SYSTEMD=ON" "-DUSE_ELOGIND=ON"
        "-DCONFIG_FILE=/etc/sddm.conf"
        ;; Set path to /etc/login.defs.
        ;; An alternative would be to use -DUID_MIN and -DUID_MAX.
        (string-append "-DLOGIN_DEFS_PATH="
                       (assoc-ref %build-inputs "shadow")
                       "/etc/login.defs")
        (string-append "-DQT_IMPORTS_DIR="
                       (assoc-ref %outputs "out") "/lib/qt5/qml")
        (string-append "-DCMAKE_INSTALL_SYSCONFDIR="
                       (assoc-ref %outputs "out") "/etc"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'embed-loginctl-reference
           (lambda _
             (substitute* "CMakeLists.txt"
               (("/usr/bin/loginctl") (which "loginctl")))
               #t)))))
    (synopsis "QML based X11 and Wayland display manager")
    (description "SDDM is a display manager for X11 and Wayland aiming to be
fast, simple and beautiful.  SDDM is themeable and puts no restrictions on the
user interface design.  It uses QtQuick which gives the designer the ability to
create smooth, animated user interfaces.")
    (home-page "https://github.com/sddm/sddm")
    ;; QML files are MIT licensed and images are CC BY 3.0.
    (license (list license:gpl2+ license:expat license:cc-by3.0))))

(define-public guix-simplyblack-sddm-theme
  (package
    (name "guix-simplyblack-sddm-theme")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plattfot/guix-simplyblack-sddm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1fwny6b0xpjs8ad2b16pyxd27gf0sr0nillmhc2h5k0q7dva21vi"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (sddm-themes (string-append out "/share/sddm/themes")))
           (mkdir-p sddm-themes)
           (copy-recursively (assoc-ref %build-inputs "source")
                             (string-append sddm-themes "/guix-simplyblack-sddm"))))))
    (home-page "https://github.com/plattfot/guix-simplyblack-sddm")
    (synopsis "Guix based theme for SDDM")
    (description
     "This package provides a simple theme for SDDM, black background with
Guix's logo.  Based on Arch linux's archlinux-simplyblack theme.")
    ;; Theme under cc-by-sa3.0, guix logo under license:cc-by-sa4.0
    (license (list license:cc-by-sa3.0 license:cc-by-sa4.0))))

(define-public chili-sddm-theme
  (package
    (name "chili-sddm-theme")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MarianArlt/sddm-chili")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "036fxsa7m8ymmp3p40z671z163y6fcsa9a641lrxdrw225ssq5f3"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (sddm-themes (string-append out "/share/sddm/themes")))
           (mkdir-p sddm-themes)
           (copy-recursively (assoc-ref %build-inputs "source")
                             (string-append sddm-themes "/chili"))))))
    (home-page "https://github.com/MarianArlt/sddm-chili")
    (synopsis "Chili theme for SDDM")
    (description "Chili reduces all the clutter and leaves you with a clean,
easy to use, login interface with a modern yet classy touch.")
    (license license:gpl3+)))

(define-public lightdm
  (package
    (name "lightdm")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/CanonicalLtd/lightdm/releases/download/"
                    version "/lightdm-" version ".tar.xz"))
              (sha256
               (base32
                "158zb2d0v1309a8v19hh32y4yj3v6yg4yg6m0l7v59d3a2b7f651"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f ; fails when run in parallel
       #:configure-flags
       (list "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda _
             (substitute* "src/shared-data-manager.c"
               (("/bin/rm") (which "rm")))
             (substitute* '("data/users.conf"
                            "common/user-list.c")
               (("/bin/false") (which "false"))
               (("/usr/sbin/nologin") (which "nologin")))
             (substitute* "src/seat.c"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'check 'pre-check
           ;; Run test-suite under a dbus session.
           (lambda* (#:key inputs #:allow-other-keys)
             (wrap-program "tests/src/test-python-greeter"
               `("PYTHONPATH"      ":" prefix (,(getenv "PYTHONPATH")))
               `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))

             ;; Avoid printing locale warnings, which trip up the text
             ;; matching tests.
             (unsetenv "LC_ALL")
             #t)))))
    (inputs
     `(("audit" ,audit)
       ("linux-pam" ,linux-pam)
       ("shadow" ,shadow)                         ;for sbin/nologin
       ("libgcrypt" ,libgcrypt)
       ("libxcb" ,libxcb)))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("itstool" ,itstool)
       ("intltool" ,intltool)
       ("vala" ,vala)                   ;for Vala bindings
       ;; For tests
       ("dbus" ,dbus)
       ("python" ,python-2)
       ("python-pygobject" ,python2-pygobject)))
    ;; Required by liblightdm-gobject-1.pc.
    (propagated-inputs
     `(("glib" ,glib)
       ("libx11" ,libx11)
       ("libxklavier" ,libxklavier)))
    (home-page "https://www.freedesktop.org/wiki/Software/LightDM/")
    (synopsis "Lightweight display manager")
    (description "The Light Display Manager (LightDM) is a cross-desktop
display manager which supports different greeters.")
    (license license:gpl3+)))

(define-public lightdm-gtk-greeter
  (package
    (name "lightdm-gtk-greeter")
    (version "2.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://launchpad.net/lightdm-gtk-greeter/"
                    (version-major+minor version) "/" version
                    "/+download/lightdm-gtk-greeter-" version ".tar.gz"))
              (sha256
               (base32
                "1g7wc3d3vqfa7mrdhx1w9ywydgjbffla6rbrxq9k3sc62br97qms"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--enable-at-spi-command="
                            (assoc-ref %build-inputs "at-spi2-core")
                            "/libexec/at-spi-bus-launcher"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-.desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append
                             out "/share/xgreeters/lightdm-gtk-greeter.desktop")
                 (("Exec=lightdm-gtk-greeter")
                  (string-append "Exec=" out "/sbin/lightdm-gtk-greeter")))
               #t)))
         (add-after 'fix-.desktop-file 'wrap-program
           ;; Mimic glib-or-gtk build system
           ;; which doesn't wrap files in /sbin
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((gtk (assoc-ref inputs "gtk+")))
               (wrap-program (string-append (assoc-ref outputs "out")
                                            "/sbin/lightdm-gtk-greeter")
                 `("XDG_DATA_DIRS" ":" prefix
                   ,(cons "/run/current-system/profile/share"
                          (map (lambda (pkg)
                                 (string-append (assoc-ref inputs pkg) "/share"))
                               '("gtk+" "shared-mime-info" "glib"))))
                 `("GTK_PATH" ":" prefix (,gtk))
                 `("GIO_EXTRA_MODULES" ":" prefix (,gtk))))
             #t)))))
    (native-inputs
     `(("exo" ,exo)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("lightdm" ,lightdm)
       ("shared-mime-info" ,shared-mime-info)
       ("at-spi2-core" ,at-spi2-core)
       ("gtk+" ,gtk+)))
    (synopsis "GTK+ greeter for LightDM")
    (home-page "https://launchpad.net/lightdm-gtk-greeter")
    (description "This package provides a LightDM greeter implementation using
GTK+, lets you select a desktop session and log in to it.")
    (license license:gpl3+)))

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
                                      "slim-reset.patch"
                                      "slim-login.patch"
                                      "slim-session.patch"
                                      "slim-sigusr1.patch"
                                      "slim-display.patch"))))
    (build-system cmake-build-system)
    (inputs `(("linux-pam" ,linux-pam)
	      ("libpng" ,libpng)
	      ("libjpeg" ,libjpeg-turbo)
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
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-new-etc-location
           (lambda _
             (substitute* "CMakeLists.txt"
               (("/etc")
                (string-append (assoc-ref %outputs "out") "/etc"))
               (("install.*systemd.*")
               ;; The build system's logic here is: if "Linux", then
                ;; "systemd".  Strip that.
                ""))
             #t)))
       #:configure-flags '("-DUSE_PAM=yes"
                           "-DUSE_CONSOLEKIT=no")
       #:tests? #f))

    ;; This used to be at <http://slim.berlios.de/>.
    (home-page "https://sourceforge.net/projects/slim.berlios/")
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
