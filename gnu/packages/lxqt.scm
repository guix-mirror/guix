;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages lxqt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))


;; Third party libraries

(define-public libdbusmenu-qt
  (package
    (name "libdbusmenu-qt")
    (version "0.9.3+16.04.20160218-0ubuntu1")
    (source
     (origin
       (method git-fetch)
       ;; Download from github rather than launchpad because launchpad trunk
       ;; tarball hash is not deterministic.
       (uri (git-reference
             (url "https://github.com/unity8-team/libdbusmenu-qt.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b7ii1cvmpcyl79gqal9c3va9m55h055s4hx7fpxkhhqs9463ggg"))))
    (build-system cmake-build-system)
    (arguments
     ;; XXX: Tests require a dbus session and some icons.
     '(#:tests? #f))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://launchpad.net/libdbusmenu-qt")
    (synopsis "Qt implementation of the DBusMenu spec")
    (description "This library provides a Qt implementation of the DBusMenu
protocol.  The DBusMenu protocol makes it possible for applications to export
and import their menus over DBus.")
    (license license:lgpl2.1+)))

(define-public libstatgrab
  (package
    (name "libstatgrab")
    (version "0.91")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ftp.i-scream.org/pub/i-scream/libstatgrab/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1azinx2yzs442ycwq6p15skl3mscmqj7fd5hq7fckhjp92735s83"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-tests")))
    (native-inputs
     ;; For testing.
     `(("perl" ,perl)))
    (home-page "https://www.i-scream.org/libstatgrab/")
    (synopsis "Provides access to statistics about the system")
    (description "libstatgrab is a library that provides cross platform access
to statistics about the system on which it's run.")
    ;; Libraries are under LGPL2.1+, and programs under GPLv2+.
    (license license:gpl2+)))


;; Base

(define-public lxqt-build-tools
  (package
    (name "lxqt-build-tools")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-build-tools/releases"
                           "/download/" version
                           "/lxqt-build-tools-" version ".tar.xz"))
       (sha256
        (base32 "0n0p0mf12n9f7zm2592779rpqrbcamfdz87nnjb8j058bc8g3214"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:configure-flags
       ;; 'startlxqt' will add LXQT_DATA_DIR to XDG_DATA_DIRS,
       ;; LXQT_ETC_XDG_DIR to XDG_CONFIG_DIRS, and 'lxqt-about' will report
       ;; LXQT_ETC_XDG_DIR in its "Technical Info".
       '("-DLXQT_DATA_DIR=/run/current-system/profile/share"
         "-DLXQT_ETC_XDG_DIR=/run/current-system/profile/etc/xdg")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib)))
    (inputs
     `(("qtbase" ,qtbase)))
    (synopsis "LXQt Build tools")
    (description
     "Lxqt-build-tools is providing several tools needed to build LXQt
itself as well as other components maintained by the LXQt project.")
    (home-page "https://lxqt.org")
    (license license:lgpl2.1+)))

(define-public libqtxdg
  (package
    (name "libqtxdg")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/libqtxdg/releases/download/"
             version "/libqtxdg-" version ".tar.xz"))
       (sha256
        (base32 "1mnnq8vbf5xjlrzajzfkay0yzzxri0zz0xi8x8rmxpw38xmglq8h"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DBUILD_TESTS=ON"
         "-DQTXDGX_ICONENGINEPLUGIN_INSTALL_PATH=lib/qt5/plugins/iconengines")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Run the tests offscreen.
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (propagated-inputs
     ;; required by Qt5XdgIconLoader.pc
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)))
    (home-page "https://github.com/lxqt/libqtxdg")
    (synopsis "Qt implementation of freedesktop.org xdg specifications")
    (description "Libqtxdg implements the freedesktop.org xdg specifications
in Qt.")
    (license license:lgpl2.1+)))

(define-public liblxqt
  (package
    (name "liblxqt")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/" name "/releases/download/"
             version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0fba0nq5b9fvvmklcikcd4nwhzlp5d6k1q1f80r34kncdzfvj7dl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "CMakeLists.txt"
               (("DESTINATION \"\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                "DESTINATION \"share/polkit-1/actions"))
             #t)))))
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("libqtxdg" ,libqtxdg)
       ("libxscrnsaver" ,libxscrnsaver)
       ("polkit-qt" ,polkit-qt)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (home-page "https://lxqt.org/")
    (synopsis "Core utility library for all LXQt components")
    (description "liblxqt provides the basic libraries shared by the
components of the LXQt desktop environment.")
    (license license:lgpl2.1+)))

(define-public libsysstat
  (package
    (name "libsysstat")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0ngz8v3bixsdg96d5ipcyxd6nsrg52974xdxy9rnimahlv1yaxn3"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (inputs
     `(("qtbase" ,qtbase)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)))
    (home-page "https://lxqt.org/")
    (synopsis "Library used to query system info and statistics")
    (description "libsysstat is a library to query system information like CPU
and memory usage or network traffic.")
    (license license:lgpl2.1+)))


;; Core

(define-public lxqt-about
  (package
    (name "lxqt-about")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "08imj7p77ifzfxnn8c482mhrvfx9gi0rb43ab5rw1rkmfvax2n5w"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (home-page "https://lxqt.org")
    (synopsis "Provides information about LXQt and the system")
    (description "lxqt-about is a dialogue window providing information about
LXQt and the system it's running on.")
    (license license:lgpl2.1+)))

(define-public lxqt-admin
  (package
    (name "lxqt-admin")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0qvpv668ja83ydbdrlal1596xhag3xlkbd6qlh9xwdpb7nysvns1"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("polkit-qt" ,polkit-qt)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "lxqt-admin-user/CMakeLists.txt"
               (("DESTINATION \"\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                "DESTINATION \"share/polkit-1/actions"))
             #t)))))
    (home-page "https://lxqt.org")
    (synopsis "LXQt system administration tool")
    (description "lxqt-admin is providing two GUI tools to adjust settings of
the operating system LXQt is running on.")
    (license license:lgpl2.1+)))

(define-public lxqt-config
  (package
    (name "lxqt-config")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0rizhl2v41kpgp57a61r6nmwcdw8nh9hprrrf33nfrdw8hpwxb95"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("libkscreen" ,libkscreen)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("libxcursor" ,libxcursor)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("src/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org")
    (synopsis "Tools to configure LXQt and the underlying operating system")
    (description "lxqt-config is providing several tools involved in the
configuration of both LXQt and the underlying operating system.")
    (license license:lgpl2.1+)))

(define-public lxqt-globalkeys
  (package
    (name "lxqt-globalkeys")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1gyvcjbhi7zpvgz1sf773dv9gc35hx5fz023njp9r4vl0dpcavgd"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("lxqt-build-tools" ,lxqt-build-tools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt"
                            "xdg/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "Daemon used to register global keyboard shortcuts")
    (description "lxqt-globalkeys is providing tools to set global keyboard
shortcuts in LXQt sessions, that is shortcuts which apply to the LXQt session
as a whole and are not limited to distinct applications.")
    (license license:lgpl2.1+)))

(define-public lxqt-notificationd
  (package
    (name "lxqt-notificationd")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1l0hdbvghyhqgvy4pih7rvz26bc6yc8a3l1bdj11hnkw62h1i7d6"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no test target
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "The LXQt notification daemon")
    (description "lxqt-notificationd is LXQt's implementation of a daemon
according to the Desktop Notifications Specification.")
    (license license:lgpl2.1+)))

(define-public lxqt-openssh-askpass
  (package
    (name "lxqt-openssh-askpass")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0l2272gya8jgv71bvg2hz37lnhiznv4ng3j0p6j79f99hwb5ygpk"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (home-page "https://lxqt.org/")
    (synopsis "GUI to query passwords on behalf of SSH agents")
    (description "lxqt-openssh-askpass is a GUI to query credentials on behalf
of other programs.")
    (license license:lgpl2.1+)))

(define-public lxqt-panel
  (package
    (name "lxqt-panel")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1qraigzq7nc5a2q6f5ybxwx07gxffa3m3bg7fiv6ppwss51xqfd1"))))
    (build-system cmake-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("kguiaddons" ,kguiaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("libdbusmenu-qt" ,libdbusmenu-qt)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("libstatgrab" ,libstatgrab)
       ("libsysstat" ,libsysstat)
       ("libxcomposite" ,libxcomposite)
       ("libxdamage" ,libxdamage)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrender" ,libxrender)
       ("lm-sensors" ,lm-sensors "lib")
       ("lxqt-globalkeys" ,lxqt-globalkeys)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)
       ("xcb-util" ,xcb-util)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt"
                            "menu/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "The LXQt desktop panel")
    (description "lxqt-panel represents the taskbar of LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-policykit
  (package
    (name "lxqt-policykit")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1d97fys6625nk4q6irp0jhsbk30xi7idnii1f3vrrrdcl2cahagp"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("pcre" ,pcre)
       ("polkit-qt" ,polkit-qt)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("polkit" ,polkit)
       ("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no test target
       #:configure-flags
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "The LXQt PolicyKit agent")
    (description "lxqt-policykit is the polkit authentification agent of
LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-powermanagement
  (package
    (name "lxqt-powermanagement")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "15nvdypyjwnp7k3d2pkhdbmaqb3ccacmh95rbdbc5mr7yrjy9613"))))
    (build-system cmake-build-system)
    (inputs
     `(("kidletime" ,kidletime)
       ("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "Power management module for LXQt")
    (description "lxqt-powermanagement is providing tools to monitor power
management events and optionally trigger actions like e. g. shut down a system
when laptop batteries are low on power.")
    (license license:lgpl2.1+)))

(define-public lxqt-qtplugin
  (package
    (name "lxqt-qtplugin")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0nnwbc99njpsyqb0cy3x0srcgwa7qrnq0qwcyx7fbvwsq1l8cz56"))))
    (build-system cmake-build-system)
    (inputs
     `(("libdbusmenu-qt" ,libdbusmenu-qt)
       ("libfm-qt" ,libfm-qt)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("src/CMakeLists.txt")
               (("DESTINATION \"\\$\\{QT_PLUGINS_DIR\\}")
                "DESTINATION \"lib/qt5/plugins"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "LXQt Qt platform integration plugin")
    (description "lxqt-qtplugin is providing a library libqtlxqt to integrate
Qt with LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-runner
  (package
    (name "lxqt-runner")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0dsg6fdcqbl7gza0sg1pb49zn5x31q7zx77jp7mkf6wc2lv8lali"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("lxqt-globalkeys" ,lxqt-globalkeys)
       ("muparser" ,muparser)
       ("pcre" ,pcre)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("lxqt-build-tools" ,lxqt-build-tools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "Tool used to launch programs quickly by typing their names")
    (description "lxqt-runner provides a GUI that comes up on the desktop and
allows for launching applications or shutting down the system.")
    (license license:lgpl2.1+)))

(define-public lxqt-session
  (package
    (name "lxqt-session")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1aibppppmg46ybbajx2qc395l0yp9rqlp2am01fqjxadsf8vci5z"))))
    (build-system cmake-build-system)
    (inputs
     `(("eudev" ,eudev)
       ("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("xdg-user-dirs" ,xdg-user-dirs)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     `(#:tests? #f
       #:configure-flags
       `("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt"
                            "config/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "Session manager for LXQt")
    (description "lxqt-session provides the standard session manager
for the LXQt desktop environment.")
    (license license:lgpl2.1+)))

(define-public lxqt-sudo
  (package
    (name "lxqt-sudo")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1j28wlw4rkzvr85yl78fqkvz7sv7dykm9ghm63xdkskfjbsas1cf"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("sudo" ,sudo)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("lxqt-build-tools" ,lxqt-build-tools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (home-page "https://lxqt.org/")
    (synopsis "GUI frontend for sudo/su")
    (description "lxqt-sudo is a graphical front-end of commands sudo and su
respectively.  As such it enables regular users to launch applications with
permissions of other users including root.")
    (license license:lgpl2.1+)))

(define-public lxqt-themes
  (package
    (name "lxqt-themes")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "13kkkzjx8bgnwckz79j273azvm4za66i4cp2qhxwdpxh0fwziklf"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)))
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_GRAPHICS_DIR\\}")
                "DESTINATION \"share/lxqt/graphics"))
             (substitute* '("themes/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_SHARE_DIR\\}")
                "DESTINATION \"share/lxqt"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "Themes, graphics and icons for LXQt")
    (description "This package comprises a number of graphic files and themes
for LXQt.")
    ;; The whole package is released under LGPL 2.1+, while the LXQt logo is
    ;; licensed under CC-BY-SA 3.0.
    (license license:lgpl2.1+)))


;; File Manager

(define-public libfm-qt
  (package
    (name "libfm-qt")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0p0lbz7dh5c38zq3yp1v1mm99ymg7mqr3h7yzniif2hipmgvxsv9"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO : prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (inputs
     `(("glib" ,glib)
       ("libexif" ,libexif)
       ("libfm" ,libfm)
       ("libxcb" ,libxcb)
       ("menu-cache" ,menu-cache)
       ("pcre" ,pcre)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (home-page "https://lxqt.org/")
    (synopsis "Qt binding for libfm")
    (description "libfm-qt is the Qt port of libfm, a library providing
components to build desktop file managers which belongs to LXDE.")
    (license license:lgpl2.1+)))

(define-public pcmanfm-qt
  (package
    (name "pcmanfm-qt")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "08jprkkk62pk34q9lxa207bh27xi86fj8jxfd5z3w2m5j5nim5mz"))))
    (build-system cmake-build-system)
    (inputs
     `(("libfm-qt" ,libfm-qt)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("lxqt-build-tools" ,lxqt-build-tools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "File manager and desktop icon manager")
    (description "PCManFM-Qt is the Qt port of PCManFM, the file manager of
LXDE.")
    (license license:gpl2+)))


;; Extra

(define-public compton-conf
  (package
    (name "compton-conf")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0q3yx2a6wf8yahrwgvhmv9sd7gmrhid528vrqy04dg8m5cx1bjci"))))
    (build-system cmake-build-system)
    (inputs
     `(("libconfig" ,libconfig)
       ("qtbase" ,qtbase)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "GUI configuration tool for compton X composite manager")
    (description "@code{compton-conf} is a configuration tool for X composite
manager Compton.")
    (license license:lgpl2.1+)))

(define-public lximage-qt
  (package
    (name "lximage-qt")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1mwjh6lrjayr9snsmabkl5qs4xm6d27mfz8k3jxfm5dk3pjj1b0w"))))
    (build-system cmake-build-system)
    (inputs
     `(("libexif" ,libexif)
       ("libfm-qt" ,libfm-qt)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (home-page "https://lxqt.org/")
    (synopsis "The image viewer and screenshot tool for lxqt")
    (description "LXImage-Qt is the Qt port of LXImage, a simple and fast
image viewer.")
    (license license:gpl2+)))

(define-public obconf-qt
  (package
    (name "obconf-qt")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1fbzn1p2mdvn8dcbavmd1imrvkph2jfssrlw8l26qz6qk8qlmhnf"))))
    (build-system cmake-build-system)
    (inputs
     `(("imlib2" ,imlib2)
       ("libsm" ,libsm)
       ("librsvg" ,librsvg)
       ("libxft" ,libxft)
       ("libxml2" ,libxml2)
       ("openbox" ,openbox)
       ("pango" ,pango)
       ("pcre" ,pcre)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (home-page "https://lxqt.org/")
    (synopsis "Openbox configuration tool")
    (description "ObConf-Qt is a Qt port of ObConf, a configuration editor for
window manager OpenBox.")
    (license license:gpl2+)))

(define-public pavucontrol-qt
  (package
    (name "pavucontrol-qt")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0pqvhhgw7d00wqw5v3ghm4l8250zy7bqpvhff6l7y1lw0z2fvcp6"))))
    (build-system cmake-build-system)
    (inputs
     `(("glib" ,glib)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (home-page "https://lxqt.org/")
    (synopsis "Pulseaudio mixer in Qt")
    (description "@code{pavucontrol-qt} is the Qt port of volume control
@code{pavucontrol} of sound server @code{PulseAudio}.")
    (license license:gpl2+)))

(define-public qps
  (package
    (name "qps")
    (version "1.10.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0pzk83p7a9ax0893s9hp2qkmiilzrf8iqry5a0x1g73hdwm5hm44"))))
    (build-system cmake-build-system)
    (inputs
     `(("libxrender" ,libxrender)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("qttools" ,qttools)))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt.org/")
    (synopsis "Qt-based visual process status monitor")
    (description "@code{qps} is a monitor that displays the status of the
processes currently in existence, much like code{top} or code{ps}.")
    (license license:gpl2+)))

(define-public qtermwidget
  (package
    (name "qtermwidget")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1c1qzbysxjbikp4bpgphphw4dgpl10gz8m06ccs2c48qxhpyd773"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (home-page "https://lxqt.org/")
    (synopsis "The terminal widget for QTerminal")
    (description "QTermWidget is a terminal emulator widget for Qt 5.")
    (license license:gpl2+)))

(define-public qterminal
  (package
    (name "qterminal")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1vlza75br1ys62lgkdz26md729bfpbnasfzswp7hakmgaq1rhms1"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("qtermwidget" ,qtermwidget)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:configure-flags
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (home-page "https://lxqt.org/")
    (synopsis "Lightweight Qt-based terminal emulator")
    (description "QTerminal is a lightweight Qt terminal emulator based on
QTermWidget.")
    (license license:gpl2+)))

(define-public screengrab
  (package
    (name "screengrab")
    (version "1.99")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/screengrab/releases/download/"
                           version "/screengrab-" version ".tar.xz"))
       (sha256
        (base32 "17y8rsx9fixvxv2byq8d6c01vry10nv07f8jy85vz7zp4f0rgzz3"))))
    (build-system cmake-build-system)
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("libqtxdg" ,libqtxdg)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt.org/")
    (synopsis "Crossplatform tool for fast making screenshots")
    (description "ScreenGrab is a program for fast creating screenshots, and
easily publishing them on internet image hosting services.")
    (license license:gpl2+)))


;; The LXQt Desktop Environment

(define-public lxqt
  (package
    (name "lxqt")
    (version (package-version lxqt-session))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     `(;; XDG
       ("desktop-file-utils" ,desktop-file-utils)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("xdg-user-dirs" ,xdg-user-dirs)
       ("xdg-utils" ,xdg-utils)
       ;; Base
       ;; TODO: qtsvg is needed for lxqt apps to display icons. Maybe it
       ;; should be added to their propagated-inputs?
       ("qtsvg" ,qtsvg)
       ;; Core
       ("lxqt-about" ,lxqt-about)
       ("lxqt-admin" ,lxqt-admin)
       ("lxqt-config" ,lxqt-config)
       ("lxqt-notificationd" ,lxqt-notificationd)
       ("lxqt-openssh-askpass" ,lxqt-openssh-askpass)
       ("lxqt-panel" ,lxqt-panel)
       ("lxqt-policykit" ,lxqt-policykit)
       ("lxqt-powermanagement" ,lxqt-powermanagement)
       ("lxqt-qtplugin" ,lxqt-qtplugin)
       ("lxqt-runner" ,lxqt-runner)
       ("lxqt-session" ,lxqt-session)
       ("lxqt-sudo" ,lxqt-sudo)
       ("lxqt-themes" ,lxqt-themes)
       ("pcmanfm-qt" ,pcmanfm-qt)
       ;; Extra
       ("compton" ,compton)
       ("compton-conf" ,compton-conf)
       ("font-dejavu" ,font-dejavu)
       ("lximage-qt" ,lximage-qt)
       ("obconf-qt" ,obconf-qt)
       ("openbox" ,openbox)
       ("oxygen-icons" ,oxygen-icons)
       ("pavucontrol-qt" ,pavucontrol-qt)
       ("qps" ,qps)
       ("qterminal" ,qterminal)))
    (synopsis "The Lightweight Qt Desktop Environment")
    (description "LXQt is a lightweight Qt desktop environment.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))
