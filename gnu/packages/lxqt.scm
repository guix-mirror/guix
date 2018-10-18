;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public libdbusmenu-qt
  (package
    (name "libdbusmenu-qt")
    (version "0.9.3+16.04.20160218-0ubuntu1")
    (source
     (origin
       (method url-fetch)
       ;; Download from github rather than launchpad beacuse launchpad trunk
       ;; tarball hash is not deterministic.
       (uri (string-append "https://github.com/unity8-team/" name
                           "/archive/" version ".tar.gz"))
       (sha256
        (base32 "0abwyggnpg50sa9cxphscp5zdkv9nxqnlav55vj21df6q1h3jb5w"))
       (file-name (string-append name "-" version ".tar.gz"))))
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
       ;; TODO: prefetch translations files from 'lxqt-l10n'.
       '("-DPULL_TRANSLATIONS=NO")))
    (inputs
     `(("glib" ,glib)
       ("libexif" ,libexif)
       ("libfm" ,libfm)
       ("libxcb" ,libxcb)
       ("menu-cache" ,menu-cache)
       ("pcre" ,pcre)
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)))
    (home-page "https://lxqt.org/")
    (synopsis "Qt binding for libfm")
    (description "libfm-qt is the Qt port of libfm, a library providing
components to build desktop file managers which belongs to LXDE.")
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
                "DESTINATION \"${CMAKE_INSTALL_PREFIX}/share/polkit-1/actions"))
             #t)))))
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("libqtxdg" ,libqtxdg)
       ("libxscrnsaver" ,libxscrnsaver)
       ("polkit-qt" ,polkit-qt)
       ("qtsvg" ,qtsvg)
       ("qttools" ,qttools)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)))
    (home-page "https://lxqt.org/")
    (synopsis "Core utility library for all LXQt components")
    (description "liblxqt provides the basic libraries shared by the
components of the LXQt desktop environment.")
    (license license:lgpl2.1+)))

(define-public libqtxdg
  (package
    (name "libqtxdg")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/" name "/releases/download/"
             version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0lq548pa69hfvnbj2ypba5ygm8n6v6g7bqqm8p5g538l1l3394cl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       `("-DBUILD_TESTS=ON"
         ,(string-append "-DQTXDGX_ICONENGINEPLUGIN_INSTALL_PATH="
                         %output "/lib/qt5/plugins/iconengines"))
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
    (home-page "https://github.com/lxqt/libqtxdg")
    (synopsis "Qt implementation of freedesktop.org xdg specifications")
    (description "Libqtxdg implements the freedesktop.org xdg specifications
in Qt.")
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
                "DESTINATION \"${CMAKE_INSTALL_PREFIX}/share/polkit-1/actions"))
             #t)))))
    (home-page "https://lxqt.org")
    (synopsis "LXQt system administration tool")
    (description "lxqt-admin is providing two GUI tools to adjust settings of
the operating system LXQt is running on.")
    (license license:lgpl2.1+)))

(define-public lxqt-build-tools
  (package
    (name "lxqt-build-tools")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "13b5x26p6ycnwzlgg1cgvlc88wjrjmlb3snrrmzh0xgh9h6hhvd6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests
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
                "DESTINATION \"${CMAKE_INSTALL_PREFIX}/etc/xdg"))
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
                "DESTINATION \"${CMAKE_INSTALL_PREFIX}/etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "Daemon used to register global keyboard shortcuts")
    (description "lxqt-globalkeys is providing tools to set global keyboard
shortcuts in LXQt sessions, that is shortcuts which apply to the LXQt session
as a whole and are not limited to distinct applications.")
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
       ("libsysstat", libsysstat)
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
                "DESTINATION \"${CMAKE_INSTALL_PREFIX}/etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "The LXQt desktop panel")
    (description "lxqt-panel represents the taskbar of LXQt.")
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
       ("qttools", qttools)
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
                "DESTINATION \"${CMAKE_INSTALL_PREFIX}/etc/xdg"))
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
       ("qttools" ,qttools)
       ("qtx11extras" ,qtx11extras)
       ("xdg-user-dirs" ,xdg-user-dirs)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)))
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
                "DESTINATION \"${CMAKE_INSTALL_PREFIX}/etc/xdg"))
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
                "DESTINATION \"${CMAKE_INSTALL_PREFIX}/etc/xdg"))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "File manager and desktop icon manager")
    (description "PCManFM-Qt is the Qt port of PCManFM, the file manager of
LXDE.")
    (license license:lgpl2.1+)))
