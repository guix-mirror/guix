;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Reza Alizadeh Majd <r.majd@pantherx.org>
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/" name "/releases/download/"
             version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1gb922npf6nw4w3nkvh4czk8xmdzzqkzq3zgl1h303fjaib359qs"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "CMakeLists.txt"
               (("DESTINATION \"\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                "DESTINATION \"share/polkit-1/actions"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
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
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0rz9w49khra9kl91kfnd3wxkldy1fqf6755mvlgbsqxb1yv8597w"))))
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "01xp5ddcxc9wvl7jm4179hjrirj07mpzm9z50936d1fqx34wfbis"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'setenv
           (lambda _
             (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org")
    (synopsis "Provides information about LXQt and the system")
    (description "lxqt-about is a dialogue window providing information about
LXQt and the system it's running on.")
    (license license:lgpl2.1+)))

(define-public lxqt-admin
  (package
    (name "lxqt-admin")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0f0skkxqyhpidpd5phliax869v4n2whvglg8rahzia2zhw4ylzry"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "lxqt-admin-user/CMakeLists.txt"
               (("DESTINATION \"\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                "DESTINATION \"share/polkit-1/actions"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("lxqt-admin-time/CMakeLists.txt"
                            "lxqt-admin-user/CMakeLists.txt")
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org")
    (synopsis "LXQt system administration tool")
    (description "lxqt-admin is providing two GUI tools to adjust settings of
the operating system LXQt is running on.")
    (license license:lgpl2.1+)))

(define-public lxqt-config
  (package
    (name "lxqt-config")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "16k36knv6d72gg8hp7423l3ic43y3l3zbaf3spqn2a354y30myrg"))))
    (build-system cmake-build-system)
    (inputs
     `(("eudev" ,eudev)
       ("kwindowsystem" ,kwindowsystem)
       ("libkscreen" ,libkscreen)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)
       ("libxcursor" ,libxcursor)
       ("libxi" ,libxi)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)
       ("xf86-input-libinput" ,xf86-input-libinput)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("src/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("lxqt-config-file-associations/CMakeLists.txt"
                            "lxqt-config-brightness/CMakeLists.txt"
                            "lxqt-config-appearance/CMakeLists.txt"
                            "lxqt-config-locale/CMakeLists.txt"
                            "lxqt-config-monitor/CMakeLists.txt"
                            "lxqt-config-input/CMakeLists.txt"
                            "liblxqt-config-cursor/CMakeLists.txt"
                            "src/CMakeLists.txt")
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org")
    (synopsis "Tools to configure LXQt and the underlying operating system")
    (description "lxqt-config is providing several tools involved in the
configuration of both LXQt and the underlying operating system.")
    (license license:lgpl2.1+)))

(define-public lxqt-globalkeys
  (package
    (name "lxqt-globalkeys")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0m6svwy20bfy9d21g5l0qzjndph3rd8zqagmqgdjzjhh3lxwrsrk"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt"
                            "xdg/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "config/CMakeLists.txt"
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1kiag3fcx12qmslln6x6lwvm4f1spymwf71389kdya3vwx7hkmcy"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("config/CMakeLists.txt"
                            "src/CMakeLists.txt")
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "The LXQt notification daemon")
    (description "lxqt-notificationd is LXQt's implementation of a daemon
according to the Desktop Notifications Specification.")
    (license license:lgpl2.1+)))

(define-public lxqt-openssh-askpass
  (package
    (name "lxqt-openssh-askpass")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1fvbgjidpifn420avh8n1gym49vcz6zgayz7xygg1x93s4awy1cs"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "GUI to query passwords on behalf of SSH agents")
    (description "lxqt-openssh-askpass is a GUI to query credentials on behalf
of other programs.")
    (license license:lgpl2.1+)))

(define-public lxqt-panel
  (package
    (name "lxqt-panel")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1r3wx0v3jm7j41h7gxr49izc9xa1afvrzq4wcdm0qbj98qa1rgpq"))))
    (build-system cmake-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("kguiaddons" ,kguiaddons)
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
    (propagated-inputs
     ;; Propagating KWINDOWSYSTEM so that the list of opened applications
     ;; shows up in lxqt-panel's taskbar plugin.
     `(("kwindowsystem" ,kwindowsystem)))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt"
                            "menu/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("cmake/BuildPlugin.cmake"
                            "panel/CMakeLists.txt")
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "The LXQt desktop panel")
    (description "lxqt-panel represents the taskbar of LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-policykit
  (package
    (name "lxqt-policykit")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "127rqb0nprybkc41lk0yq0r0dk5pbbw22gvrm4pwag71qh8wpk5i"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "The LXQt PolicyKit agent")
    (description "lxqt-policykit is the polkit authentication agent of
LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-powermanagement
  (package
    (name "lxqt-powermanagement")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "06bvgbkbl9p9n8ba5cfsynqgmpb5c8yfnsvp7zqhflj8k9p9msip"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("config/CMakeLists.txt"
                            "src/CMakeLists.txt")
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
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
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "18y7xfxwyismcycg70q6r8zrcygz1pdcvg6lqc6ba7azqb9806ds"))))
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1c687shypivkhjrpzs1jcy5l2i8d7xzm31c4is1xx6x9nbkgm4bm"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "Tool used to launch programs quickly by typing their names")
    (description "lxqt-runner provides a GUI that comes up on the desktop and
allows for launching applications or shutting down the system.")
    (license license:lgpl2.1+)))

(define-public lxqt-session
  (package
    (name "lxqt-session")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "11i2vimv3336dvvxb6y5csdybwjncr7cq3kwlj52vkpisnxslvgy"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("autostart/CMakeLists.txt"
                            "config/CMakeLists.txt")
               (("DESTINATION \"\\$\\{LXQT_ETC_XDG_DIR\\}")
                "DESTINATION \"etc/xdg"))
             #t))
         ;; add write permission to lxqt-rc.xml file which is stored as read-only in store
         (add-after 'unpack 'patch-openbox-permission
           (lambda _
             (substitute* "startlxqt.in"
               (("cp \"\\$LXQT_DEFAULT_OPENBOX_CONFIG\" \"\\$XDG_CONFIG_HOME/openbox\"")
                 (string-append "cp \"$LXQT_DEFAULT_OPENBOX_CONFIG\" \"$XDG_CONFIG_HOME/openbox\"\n"
                                "        # fix openbox permission issue\n"
                                "        chmod u+w  \"$XDG_CONFIG_HOME/openbox\"/*")))
             #t))
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("lxqt-config-session/CMakeLists.txt"
                            "lxqt-leave/CMakeLists.txt"
                            "lxqt-session/CMakeLists.txt")
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "Session manager for LXQt")
    (description "lxqt-session provides the standard session manager
for the LXQt desktop environment.")
    (license license:lgpl2.1+)))

(define-public lxqt-sudo
  (package
    (name "lxqt-sudo")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0mbygp2kkppwk7sxvpnwavdwrx88mh7ldcg6xm3zw1ndp29danay"))))
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
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-translations-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/lxqt/translations")))
             #t)))))
    (home-page "https://lxqt.org/")
    (synopsis "GUI frontend for sudo/su")
    (description "lxqt-sudo is a graphical front-end of commands sudo and su
respectively.  As such it enables regular users to launch applications with
permissions of other users including root.")
    (license license:lgpl2.1+)))

(define-public lxqt-themes
  (package
    (name "lxqt-themes")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0p7svdpp0z44hvgrp2aip6hym0gdhbanyxsz6iz8sjnn28c995ia"))))
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "06p3wqpc574v9f94wkq9hqmbbvb9q8phfpq301z55c5r939f4hrp"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no tests
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0x3c25inlxll965xszx37mnl5gp3smm2h7x04f67z0qlh3vsbrjq"))))
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "11n8k59jd0q2x66cispc9dpk139mp6j99hq1yjccxvh21vhc7mbc"))))
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0iiq55rm4z2jp19q1pbd2whifwvxg052q324vrwp4p7nz0wh04za"))))
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
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt.org/")
    (synopsis "The image viewer and screenshot tool for lxqt")
    (description "LXImage-Qt is the Qt port of LXImage, a simple and fast
image viewer.")
    (license license:gpl2+)))

(define-public obconf-qt
  (package
    (name "obconf-qt")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1kzb7364150b60qd3wcgnw78b9ia5k3b16kq8w3p1y7pg6pddy8m"))))
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
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt.org/")
    (synopsis "Openbox configuration tool")
    (description "ObConf-Qt is a Qt port of ObConf, a configuration editor for
window manager OpenBox.")
    (license license:gpl2+)))

(define-public pavucontrol-qt
  (package
    (name "pavucontrol-qt")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "18mw5r8grfrf95vxjbqrr790kl5l59qdqcmlxmwa6rwbfgywj1fq"))))
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
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt.org/")
    (synopsis "Pulseaudio mixer in Qt")
    (description "@code{pavucontrol-qt} is the Qt port of volume control
@code{pavucontrol} of sound server @code{PulseAudio}.")
    (license license:gpl2+)))

(define-public qps
  (package
    (name "qps")
    (version "1.10.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0dz7ricxg2rrmdyca6mc2d4lyy5bpksjk751hvn95wssr76y2w0m"))))
    (build-system cmake-build-system)
    (inputs
     `(("libxrender" ,libxrender)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
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
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0v1vvi8vf9y8nv8y0gzffaqji53s75ab5jypksih0ndcws8ryww4"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt.org/")
    (synopsis "The terminal widget for QTerminal")
    (description "QTermWidget is a terminal emulator widget for Qt 5.")
    (license license:gpl2+)))

(define-public qterminal
  (package
    (name "qterminal")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0cgyaskyqginmm85d11inbi0mmxrsrnvgyx6g4l4l4iqpphfq670"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("qtermwidget" ,qtermwidget)))
    (native-inputs
     `(("lxqt-build-tools" ,lxqt-build-tools)
       ("qttools" ,qttools)))
    (arguments
     '(#:tests? #f))                      ; no tests
    (home-page "https://lxqt.org/")
    (synopsis "Lightweight Qt-based terminal emulator")
    (description "QTerminal is a lightweight Qt terminal emulator based on
QTermWidget.")
    (license license:gpl2+)))

(define-public screengrab
  (package
    (name "screengrab")
    (version "1.101")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/screengrab/releases/download/"
                           version "/screengrab-" version ".tar.xz"))
       (sha256
        (base32 "05f81xjlmiykd7iwx5xns5vnynjq4js4x1bk8wd648frrksp44fa"))))
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
