;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages kde)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public baloo-widgets
  (package
    (name "baloo-widgets")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/baloo-widgets-" version ".tar.xz"))
       (sha256
        (base32 "0bba8dgxd7rcjji809kwnw78hl1nb5ssh2ir4k4b0wvx395jifgd"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("baloo" ,baloo)
       ("kconfig" ,kconfig)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Baloo")
    (synopsis "Wigets for use with Baloo")
    (description "Baloo is a framework for searching and managing metadata.
This package contains GUI widgets for baloo.")
    (license license:lgpl2.0+)))

(define-public grantleetheme
  (package
    (name "grantleetheme")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/grantleetheme-" version ".tar.xz"))
       (sha256
        (base32 "0j77q1yyfmggzgkqgcw2xr0v9xg3h5cdhh8jry8h2llw75ahy6xb"))
       (patches (search-patches "grantlee-merge-theme-dirs.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("libxml2" ,libxml2))) ;; xmllint required for tests
    (inputs
     `(("grantlee" ,grantlee)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("knewstuff" ,knewstuff)
       ("qtbase" ,qtbase)))
    (home-page "https://cgit.kde.org/grantleetheme.git")
    (synopsis "Library providing Grantlee theme support")
    (description "This library provides Grantlee theme support.")
    (license ;; LGPL for libraries, FDL for documentation
     (list license:lgpl2.0+ license:fdl1.2+))))

(define-public kdenlive
  (let ((version "18.08.1"))
    (package
      (name "kdenlive")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://anongit.kde.org/kdenlive.git")
               (commit (string-append "v" version))))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0ifnaclsz7w08mc485i3j1kkcpd1m8q5qamckrfwc375ac13xf4g"))))
      (build-system cmake-build-system)
      (native-inputs
       `(("extra-cmake-modules" ,extra-cmake-modules)
         ("qttools" ,qttools)))
      (propagated-inputs
       `(("mlt" ,mlt)))
      (inputs
       `(("shared-mime-info" ,shared-mime-info)
         ("frei0r-plugins" ,frei0r-plugins)
         ("qtbase" ,qtbase)
         ("qtscript" ,qtscript)
         ("qtsvg" ,qtsvg)
         ("kparts" ,kparts)
         ("knotifications" ,knotifications)
         ("karchive" ,karchive)
         ("kdbusaddons" ,kdbusaddons)
         ("kcrash" ,kcrash)
         ("kguiaddons" ,kguiaddons)
         ("knewstuff" ,knewstuff)
         ("knotifyconfig" ,knotifyconfig)
         ("kfilemetadata" ,kfilemetadata)
         ("kdoctools" ,kdoctools)
         ("kdeclarative" ,kdeclarative)
         ("qtdeclarative" ,qtdeclarative)
         ("qtquickcontrols" ,qtquickcontrols)
         ("kiconthemes" ,kiconthemes)
         ("qtgraphicaleffects" ,qtgraphicaleffects)
         ("kplotting" ,kplotting)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (qtquickcontrols (assoc-ref inputs "qtquickcontrols"))
                      (qtbase (assoc-ref inputs "qtbase"))
                      (qtdeclarative (assoc-ref inputs "qtdeclarative"))
                      (frei0r (assoc-ref inputs "frei0r-plugins"))
                      (qml "/lib/qt5/qml"))
                 (wrap-program (string-append out "/bin/kdenlive")
                   `("QT_PLUGIN_PATH" ":" prefix
                     ,(map (lambda (label)
                             (string-append (assoc-ref inputs label)
                                            "/lib/qt5/plugins/"))
                           '("qtbase" "qtsvg")))
                   `("FREI0R_PATH" ":" =
                     (,(string-append frei0r "/lib/frei0r-1/")))
                   `("QT_QPA_PLATFORM_PLUGIN_PATH" ":" =
                     (,(string-append qtbase "/lib/qt5/plugins/platforms")))
                   `("QML2_IMPORT_PATH" ":" prefix
                     (,(string-append qtquickcontrols qml)
                      ,(string-append qtdeclarative qml)))))
               #t)))))
      (home-page "https://kdenlive.org")
      (synopsis "Non-linear video editor")
      (description "Kdenlive is an acronym for KDE Non-Linear Video Editor.

Non-linear video editing is much more powerful than beginner's (linear)
editors, hence it requires a bit more organization before starting.  However,
it is not reserved to specialists and can be used for small personal
projects.")
      (license license:gpl2+))))

(define-public kdevelop
  (package
    (name "kdevelop")
    (version "5.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/kdevelop"
                            "/" version "/src/kdevelop-"
                            version ".tar.xz"))
        (sha256
         (base32 "01jmrmwbc1hrvq7jdfcc7mxl03q2l6kz57yca2j26xwyvfcfv5sz"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("shared-mime-info" ,shared-mime-info)
       ("qttools" ,qttools)))
    (inputs
     `(("boost" ,boost)
       ("clang" ,clang)
       ("grantlee" ,grantlee)
       ("karchive" ,karchive)
       ("kcmutils" ,kcmutils)
       ("kcrash" ,kcrash)
       ("kdeclarative" ,kdeclarative)
       ("kdoctools" ,kdoctools)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)  ;; not checked as requirement
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kparts" ,kparts)
       ("kservice" ,kservice)
       ("ktexteditor" ,ktexteditor)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libkomparediff2" ,libkomparediff2)
       ("oxygen-icons" ,oxygen-icons)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols" ,qtquickcontrols)  ;; not checked as requirement
       ("qtquickcontrols2" ,qtquickcontrols2)  ;; not checked as requirement
       ("qtwebkit" ,qtwebkit)
       ("threadweaver" ,threadweaver)

       ;; recommendes
       ("astyle" ,astyle)
       ("kdevelop-pg-qt" ,kdevelop-pg-qt)
       ("libksysguard" ,libksysguard)

       ;; optional
       ("apr" ,apr)            ; required for subversion support
       ("apr-util" ,apr-util)  ; required for subversion support
       ("attica" ,attica)
       ("kconfigwidgets" ,kconfigwidgets)
       ("knewstuff" ,knewstuff)
       ("krunner" ,krunner)
       ;; TODO: OktetaGui, OktetaKastenControllers
       ("plasma" ,plasma-framework)
       ;; TODO: purpose
       ("sonnet" ,sonnet)
       ("subversion" ,subversion)))

       ;; run-time packages - TODO
       ;; ClazyStandalone
       ;; Cppcheck
       ;; heaptrack
       ;; heaptrack_gui
       ;; meson
    (arguments
     `(#:tests? #f  ;; there are some issues with the test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-include-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "cmake/modules/FindClang.cmake"
               (("^\\s*PATHS \"\\$\\{CLANG_LIBRARY_DIRS\\}\"" line)
                (string-append line " " (assoc-ref inputs "clang") "/lib")))
             #t)))))
    (home-page "https://kdevelop.org")
    (synopsis "IDE for C, C++, Python, Javascript and PHP")
    (description "The KDevelop IDE provides semantic syntax highlighting, as
well as code navigation and completion for C, C++ (using Clang/LLVM), QML,
JavaScript, Python and PHP.  It also integrates with a debugger, different
build systems (CMake, QMake, custom Makefiles) and version control
software (Git, Subversion, Mercurial, CVS and Bazaar).")
    (license license:lgpl2.1+)))

(define-public kdevelop-pg-qt
  (package
    (name "kdevelop-pg-qt")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/KDE/kdevelop-pg-qt/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15ja19gg6x7gww4ch12hy585x55ghbkpsiyr8fqiyjk0j6v07hh5"))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (build-system cmake-build-system)
    (home-page "https://kde.org")
    (synopsis "Parser generator library for KDevplatform")
    (description "KDevelop-PG-Qt is the parser generator used in KDevplatform
for some KDevelop language plugins (Ruby, PHP, CSS...).")
    (license license:lgpl2.0+)))

;; kdevplatform was merged into kdevelop as of 5.2.x
(define-deprecated kdevplatform kdevelop)

(define-public krita
  (package
    (name "krita")
    (version "4.2.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/krita/" version
                    "/krita-" version ".tar.gz"))
              (sha256
               (base32
                "0gcwq1w09gmx53i2fir73l222p41299wagvhbvsxwrz0v3crzliy"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "-DBUILD_TESTING=OFF"
             (string-append "-DCMAKE_CXX_FLAGS=-I"
                            (assoc-ref %build-inputs "ilmbase")
                            "/include/OpenEXR"))
       #:phases
       (modify-phases %standard-phases
         ;; Ensure that icons are found at runtime.
         ;; This works around <https://bugs.gnu.org/22138>.
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (qt '("qtbase" "qtsvg")))
               (wrap-program (string-append out "/bin/krita")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins/"))
                         qt)))
               #t))))))
    (native-inputs
     `(("curl" ,curl)
       ("eigen" ,eigen)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("gettext-minimal" ,gettext-minimal)
       ("kitemmodels" ,kitemmodels)
       ("pkg-config" ,pkg-config)
       ("qwt" ,qwt)
       ("vc" ,vc)))
    (inputs
     `(("boost" ,boost)
       ("exiv2" ,exiv2)
       ("fftw" ,fftw)
       ("gsl" ,gsl)
       ("ilmbase" ,ilmbase)
       ("karchive" ,karchive)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("lcms" ,lcms)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libraw" ,libraw-0.18)
       ("libtiff" ,libtiff)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxi" ,libxi)
       ("openexr" ,openexr)
       ("perl" ,perl)
       ("poppler-qt5" ,poppler-qt5)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("quazip" ,quazip)
       ("zlib" ,zlib)))
    (home-page "https://krita.org")
    (synopsis "Digital painting application")
    (description
     "Krita is a professional painting tool designed for concept artists,
illustrators, matte and texture artists, and the VFX industry.  Notable
features include brush stabilizers, brush engines and wrap-around mode.")
    (license license:gpl2+)))

(define-public libkomparediff2
  (package
    (name "libkomparediff2")
    (version "19.08.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/applications/" version
                            "/src/libkomparediff2-" version ".tar.xz"))
        (sha256
         (base32 "1mvihd0xpkl8kryf5dvsfgpbgs9af8c9bzq8mmr74gfsvfb8ywy5"))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (build-system cmake-build-system)
    (home-page "https://kde.org")
    (synopsis "Library to compare files and strings, used in Kompare and KDevelop")
    (description "Libkomparediff2 is a library to work with diffs and patches,
used in KDE development tools Kompare and KDevelop.")

    ;; GPL, some files are also licensed under LGPL or BSD, see COPYING in the
    ;; source archive
    (license (list license:gpl2+ license:lgpl2.0+ license:bsd-3))))

(define-public qca
  (package
    (name "qca")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/qca/" version
                            "/qca-" version ".tar.xz"))
        (sha256
         (base32
          "00kv1vsrc8fp556hm8s6yw3240vx3l4067q6vfxrb3gdwgcd45np"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)
       ("qtbase" ,qtbase)))
    (home-page "https://userbase.kde.org/QCA")
    (synopsis "Libraries for the Qt Cryptographic Architecture")
    (description "The Qt Cryptographic Architecture (QCA) provides a
straightforward and cross-platform API for a range of cryptographic features,
including SSL/TLS, X.509 certificates, SASL, OpenPGP, S/MIME CMS, and smart
cards.")
    (license license:lgpl2.1)))

(define-public kpmcore
  (package
    (name "kpmcore")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/kpmcore"
                    "/" version "/src/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1sslkwcj2cyrn7bpjdjdwikp1q8wrsxpsg2sxxd8hsairgy7ygh3"))
              (patches (search-patches "kpmcore-fix-tests.patch"
                                       "kpmcore-remove-broken-test.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kauth" ,kauth)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qca" ,qca)
       ("util-linux" ,util-linux)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Library for managing partitions")
    (description "Library for managing partitions.")
    (license license:gpl3+)))

(define-public snorenotify
  (package
    (name "snorenotify")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/snorenotify/"
                            version "/src/snorenotify-" version ".tar.xz"))
        (sha256
         (base32
          "0jz6ivk90h7iwgyxar7xzzj8yvzn6s1my6cqs9bdnwqswfk1nhbd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; both tests fail, require display
    (inputs
     `(("qtbase" ,qtbase)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (home-page "https://techbase.kde.org/Projects/Snorenotify")
    (synopsis "Qt notification framework")
    (description "Snorenotify is a multi platform Qt notification framework.
Using a plugin system it is possible to create notifications with many
different notification systems.")
    (license license:lgpl3)))

(define-public kdeconnect
  (package
    (name "kdeconnect")
    (version "1.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/kdeconnect/"
                            version "/kdeconnect-kde-"
                            version ".tar.xz"))
        (sha256
         (base32
          "02lr3xx5s2mgddac4n3lkgr7ppf1z5m6ajs90rjix0vs8a271kp5"))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_TESTING=ON")
       #:tests? #f)) ; tests fail hard in our build environment
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("libxtst" ,libxtst)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("kcmutils" ,kcmutils)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("knotifications" ,knotifications)
       ("kwayland" ,kwayland)
       ("libfakekey" ,libfakekey)
       ("qca" ,qca)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://community.kde.org/KDEConnect")
    (synopsis "Enable your devices to communicate with each other")
    (description "KDE Connect is a project that enables all your devices to
communicate with each other.  Here's a few things KDE Connect can do:
@enumerate
@item Receive your phone notifications on your desktop computer and reply to messages
@item Control music playing on your desktop from your phone
@item Use your phone as a remote control for your desktop
@item Run predefined commands on your PC from connected devices
@item Check your phones battery level from the desktop
@item Ring your phone to help finding it
@item Share files and links between devices
@item Browse your phone from the desktop
@item Control the desktop's volume from the phone
@end enumerate")
    (properties `((upstream-name . "kdeconnect-kde")))
    (license (list license:gpl2 license:gpl3)))) ; dual licensed

(define-public kqtquickcharts
  (package
    (name "kqtquickcharts")
    (version "19.08.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/applications/"
                            version "/src/kqtquickcharts-" version ".tar.xz"))
        (sha256
         (base32
          "1yy9fyd8y4g25ljdsbil19qdf4j3mzmzl489sx7rqpm3lfdzjh9k"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://phabricator.kde.org/source/kqtquickcharts/")
    (synopsis "Interactive charts for Qt Quick")
    (description
     "Kqtquickcharts is a QtQuick plugin to render beautiful and interactive
charts.")
    (license license:lgpl2.1+)))

(define-public kcachegrind
  (package
    (name "kcachegrind")
    (version "19.04.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/applications/" version
                                  "/src/kcachegrind-" version ".tar.xz"))
              (sha256
               (base32
                "1hhsk64yp6q2xh8j269j4wp9y24ggmii861r6gf02mj1mbn2p1jb"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("perl" ,perl)
       ("python" ,python)
       ("qttools" ,qttools)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("karchive" ,karchive)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kdbusaddons" ,kdbusaddons)))
    ;; Note: The 'hotshot2calltree' and 'pprof2calltree' scripts depend on
    ;; Python and PHP, respectively.  These are optional and we ignore them
    ;; for now.
    (home-page "https://kcachegrind.github.io/html/Home.html")
    (synopsis "Visualize profiles produces by Valgrind's Cachegrind tool")
    (description
     "The data files generated by the Callgrind of Valgrind, an application
profiler, can be loaded into KCachegrind for browsing the performance results.
There is also a command-line tool to get ASCII reports from data files without
the need to use KCachegrind.

The format of Callgrind output is documented.  With conversion scripts,
KCachegrind is able to visualize output of other profilers like OProfile, a
system-wide profiler for Linux using statistical sampling with hardware
performance counters.  There also exist converters for profiling output of
Python, PHP, and Perl.")
    (license license:gpl2)))

(define-public libkdegames
  (package
    (name "libkdegames")
    (version "19.08.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/applications/" version
                          "/src/libkdegames-" version ".tar.xz"))
      (sha256
       (base32 "12dvkmjgbi8dp9y55zmx1pw3zr2i374c4vn3mfn9r31bf06dr701"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kdnssd" ,kdnssd)
       ("kglobalaccel" ,kglobalaccel)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ;("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("knewstuff" ,knewstuff)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("libsndfile" ,libsndfile)
       ("openal" ,openal)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)))
    (home-page "https://games.kde.org/")
    (synopsis "Runtime library for kdegames")
    (description "Runtime library for kdegames")
    (license (list license:gpl2+  license:fdl1.2+))))

(define-public zeroconf-ioslave
  (package
    (name "zeroconf-ioslave")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/zeroconf-ioslave-" version ".tar.xz"))
       (sha256
        (base32 "1vbi4kpyrk530q2dj8ql2i0gxjybdbmkqpl8vkhrihl7r7f0xc5p"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kdbusaddons" ,kdbusaddons)
       ("kdnssd" ,kdnssd)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/internet/org.kde.zeroconf_ioslave")
    (synopsis "DNS-SD Service Discovery Monitor")
    (description "Adds an entry to Dolphin's Network page to show local
services such as printers which advertise themselves with DNSSD (called Avahi
or Bonjour by other projects).")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))
