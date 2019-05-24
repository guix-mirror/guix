;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages boost)
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
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

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
    (version "5.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/kdevelop"
                            "/" version "/src/kdevelop-"
                            version ".tar.xz"))
        (sha256
         (base32
          "1iqaq0ilijjigqb34v5wq9in6bnjs0p9cmgbygjmy53xhh3yhm5g"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("kdevplatform" ,kdevplatform)
       ("kdevelop-pg-qt" ,kdevelop-pg-qt)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtwebkit" ,qtwebkit)
       ("karchive" ,karchive)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kdeclarative" ,kdeclarative)
       ("kdoctools" ,kdoctools)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kiconthemes" ,kiconthemes)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("knotifyconfig" ,knotifyconfig)
       ("knotifications" ,knotifications)
       ("kparts" ,kparts)
       ("kcrash" ,kcrash)
       ("knewstuff" ,knewstuff)
       ("krunner" ,krunner)
       ("kxmlgui" ,kxmlgui)
       ("libksysguard" ,libksysguard)
       ("threadweaver" ,threadweaver)
       ("ktexteditor" ,ktexteditor)
       ("kwindowsystem" ,kwindowsystem)
       ("plasma" ,plasma-framework)
       ("grantlee" ,grantlee)
       ("libepoxy" ,libepoxy)
       ("clang" ,clang)
       ("shared-mime-info" ,shared-mime-info)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check) ;; there are some issues with the test suite
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (kdevplatform (assoc-ref inputs "kdevplatform"))
                    (kio (assoc-ref inputs "kio"))
                    (kcmutils (assoc-ref inputs "kcmutils"))
                    (qtquickcontrols (assoc-ref inputs "qtquickcontrols"))
                    (qtbase (assoc-ref inputs "qtbase"))
                    (qtdeclarative (assoc-ref inputs "qtdeclarative"))
                    (qml "/qml"))
               (wrap-program (string-append out "/bin/kdevelop")
                 `("XDG_DATA_DIRS" ":" prefix
                   ,(map (lambda (s) (string-append s "/share"))
                         (list out kdevplatform kcmutils)))
                 `("QT_QPA_PLATFORM_PLUGIN_PATH" ":" =
                   (,(string-append qtbase "/plugins/platforms")))
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (s) (string-append s "/lib/plugins"))
                         (list out kdevplatform kio)))
                 `("QML2_IMPORT_PATH" ":" prefix
                   (,(string-append qtquickcontrols qml)
                    ,(string-append qtdeclarative qml))))))))))
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/KDE/kdevelop-pg-qt/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1av8plqz7hyhrd07avnmn6ryslqlarmxn0pw7swzvb6ddiqp59j4"))))
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

(define-public kdevplatform
  (package
    (name "kdevplatform")
    (version "5.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/kdevelop"
                                  "/" version "/src/kdevplatform-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0jk6g1kiqpyjy8pca0236b9944gxqnymqv8ny6m8nrraannxs8p6"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("apr" ,apr)
       ("apr-util" ,apr-util)
       ("boost" ,boost)
       ("karchive" ,karchive)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcmutils" ,kcmutils)
       ("kiconthemes" ,kiconthemes)
       ("kdeclarative" ,kdeclarative)
       ("kdoctools" ,kdoctools)
       ("kguiaddons" ,kguiaddons)
       ("kinit" ,kinit)
       ("kitemmodels" ,kitemmodels)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kwindowsystem" ,kwindowsystem)
       ("kio" ,kio)
       ("ki18n" ,ki18n)
       ("kparts" ,kparts)
       ("kservice" ,kservice)
       ("grantlee" ,grantlee)
       ("libkomparediff2" ,libkomparediff2)
       ("sonnet" ,sonnet)
       ("threadweaver" ,threadweaver)
       ("ktexteditor" ,ktexteditor)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("qtwebkit" ,qtwebkit)
       ("qtx11extras" ,qtx11extras)
       ("plasma" ,plasma-framework)
       ("subversion" ,subversion)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
               (setenv "QT_PLUGIN_PATH"
                       (string-append out "/lib/plugins:"
                                      (getenv "QT_PLUGIN_PATH")))
               (setenv "XDG_DATA_DIRS"
                       (string-append out "/share:"
                                      (getenv "XDG_DATA_DIRS")))
               (invoke "ctest" "-R" ; almost all tests require a display
                       "filteringstrategy|kdevvarlengtharray|kdevhash")))))))
    (home-page "https://github.com/KDE/kdevplatform")
    (synopsis "Framework to build integrated development environments (IDEs)")
    (description "KDevPlatform is the basis of KDevelop and contains some
plugins, as well as code to create plugins, or complete applications.")
    (license license:gpl3+)))

(define-public krita
  (package
    (name "krita")
    (version "4.1.7.101")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/krita/"
                    (version-prefix version 3)
                    "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pvghb17vj3y19wa1n1zfg3yl5206ir3y45znrgdgdw076m5pjav"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "-DBUILD_TESTING=OFF" "-DKDE4_BUILD_TESTS=OFF"
             (string-append "-DWITH_FFTW3="
                            (assoc-ref %build-inputs "fftw"))
             (string-append "-DWITH_GSL="
                            (assoc-ref %build-inputs "gsl"))
             (string-append "-DWITH_LibRaw="
                            (assoc-ref %build-inputs "libraw"))
             (string-append "-DWITH_TIFF="
                            (assoc-ref %build-inputs "libtiff"))
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
       ("qwt" ,qwt)
       ("vc" ,vc)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtx11extras" ,qtx11extras)
       ("qtsvg" ,qtsvg)
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
       ("boost" ,boost)
       ("exiv2" ,exiv2)
       ("lcms" ,lcms)
       ("libpng" ,libpng)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("zlib" ,zlib)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxi" ,libxi)
       ("fftw" ,fftw)
       ("gsl" ,gsl)
       ("poppler-qt5" ,poppler-qt5)
       ("libraw" ,libraw-0.18)
       ("libtiff" ,libtiff)
       ("perl" ,perl)
       ("ilmbase" ,ilmbase)
       ("openexr" ,openexr)))
    (home-page "https://krita.org")
    (synopsis "Digital painting application")
    (description
     "Krita is a professional painting tool designed for concept artists,
illustrators, matte and texture artists, and the VFX industry.  Notable
features include brush stabilizers, brush engines and wrap-around mode.")
    (license license:gpl2+)))

(define-public kholidays
  (package
    (name "kholidays")
    (version "17.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/applications/" version "/src/"
             name "-" version ".tar.xz"))
       (sha256
        (base32 "0595d7wbnz8kyq1bnivdrp20lwdp8ykvdll1fmb0fgm4q24z0cl8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; blacklist a failing test function TODO: make it pass
             (with-output-to-file "autotests/BLACKLIST"
               (lambda _
                 (display "[testDefaultRegions]\n*\n")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://cgit.kde.org/kholidays.git")
    (synopsis "Library for regional holiday information")
    (description "This library provides a C++ API that determines holiday and
other special events for a geographical region.")
    (license license:lgpl2.0+)))

(define-public libkomparediff2
  (package
    (name "libkomparediff2")
    (version "19.04.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/applications/" version
                            "/src/libkomparediff2-" version ".tar.xz"))
        (sha256
         (base32 "1cyi7a5ss7jv87llk0k8c9g3h1qsp6j6nmdzh3xxcswr4p5skc9a"))))
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
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/qca/" version
                            "/src/qca-" version ".tar.xz"))
        (sha256
         (base32
          "0lz3n652z208daxypdcxiybl0a9fnn6ida0q7fh5f42269mdhgq0"))))
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
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qtbase" ,qtbase)
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
    (version "1.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/kdeconnect/"
                            version "/src/kdeconnect-kde-"
                            version ".tar.xz"))
        (sha256
         (base32
          "1vac0mw1myrswr61adv7lgif0c4wzw5wnsj0sqxj6msp4l4pfgsg"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_TESTING=ON")
       #:tests? #f ; tests fail hard in our build environment
       #:modules ((guix build cmake-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%cmake-build-system-modules
                            (guix build qt-utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "QT_PLUGIN_PATH"
                       (string-append out "/lib/qt5/plugins"
                                      ":" (getenv "QT_PLUGIN_PATH")))
               (wrap-qt-program out "../lib/libexec/kdeconnectd")
               (wrap-qt-program out "kdeconnect-cli")
               (wrap-qt-program out "kdeconnect-handler")
               (wrap-qt-program out "kdeconnect-indicator"))
             #t)))))
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
    (license (list license:gpl2 license:gpl3)))) ; dual licensed
