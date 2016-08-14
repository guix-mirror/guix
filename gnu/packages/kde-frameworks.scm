;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
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

(define-module (gnu packages kde-frameworks)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public extra-cmake-modules
  (package
    (name "extra-cmake-modules")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "01m12ml529pwr2sal951r5z6yb1rwbpid1y4k14nlk3xqgmdakwa"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("qtbase" ,qtbase))) ; For tests (needs qmake)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; install and check phase are swapped to prevent install from failing
         ;; after testsuire has run
         (add-after 'install 'check-post-install
           (assoc-ref %standard-phases 'check))
         (delete 'check))))
    ;; optional dependencies - to save space, we do not add these inputs.
    ;; Sphinx > 1.2:
    ;;   Required to build Extra CMake Modules documentation in Qt Help format.
    ;; Qt5LinguistTools , Qt5 linguist tools. , <http://www.qt.io/>
    ;;   Required to run tests for the ECMPoQmTools module.
    ;; Qt5Core
    ;;   Required to run tests for the ECMQtDeclareLoggingCategory module,
    ;;   and for some tests of the KDEInstallDirs module.
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "CMake module files for common software used by KDE")
    (description "The Extra CMake Modules package, or ECM, adds to the
modules provided by CMake to find common software.  In addition, it provides
common build settings used in software produced by the KDE community.")
    (license license:bsd-3)))

(define-public phonon
  (package
    (name "phonon")
    (version "4.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/phonon"
                            "/" version "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1q5hvsk4sfcb91625wcmldy7kgjmfpmpmkgzi6mxkqdd307v8x5v"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:configure-flags
       '("-DCMAKE_CXX_FLAGS=-fPIC"
         "-DPHONON_BUILD_PHONON4QT5=ON")))
    (home-page "https://phonon.kde.org")
    (synopsis "KDE's multimedia library")
    (description "KDE's multimedia library.")
    (license license:lgpl2.1+)))

(define-public gpgmepp
  (package
    (name "gpgmepp")
    (version "16.04.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/applications"
                            "/" version "/src/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1850pdysi7c1w0nxnhcbrhnkrfqyrcl0laxyjcw1g1ln764pwcmj"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (propagated-inputs
     `(("boost" ,boost)
       ("gpgme" ,gpgme)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "C++ bindings/wrapper for gpgme")
    (description "C++ bindings/wrapper for gpgme.")
    (license license:lgpl2.1+)))

(define-public kpmcore
  (package
    (name "kpmcore")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/kpmcore"
                            "/" version "/src/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1blila6ncqbmzhycx3szrbkxc000pzh62956mw5ihxvhrqpncg2p"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kconfigwidgets" ,kconfigwidgets)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("ki18n" ,ki18n)
       ("kservice" ,kservice)
       ("libatasmart" ,libatasmart)
       ("parted" ,parted)
       ("qtbase" ,qtbase)
       ("util-linux" ,util-linux)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Library for managing partitions")
    (description "Library for managing partitions.")
    (license license:gpl3+)))


;; Tier 1
;;
;; Tier 1 frameworks depend only on Qt (and possibly a small number of other
;; third-party libraries), so can easily be used by an Qt-based project.

(define-public attica
  (package
    (name "attica")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0d368gmds7m7k5pnn625wqsij38cvxk1gkm4zv24phnk9f67v7cw"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Open Collaboration Service client library")
    (description "Attica is a Qt library that implements the Open
Collaboration Services API version 1.6.

It grants easy access to the services such as querying information about
persons and contents.  The library is used in KNewStuff3 as content provider.
In order to integrate with KDE's Plasma Desktop, a platform plugin exists in
kdebase.

The REST API is defined here:
http://freedesktop.org/wiki/Specifications/open-collaboration-services/")
    (license (list license:lgpl2.1+ license:lgpl3+))))

(define-public bluez-qt
  (package
    (name "bluez-qt")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0gy0m7lcwwklf021l5i3v7j0cl7qz7cgvzrwpj87ix3kyw5xs80z"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:configure-flags
        '("-DINSTALL_UDEV_RULE:BOOL=OFF")
       #:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* _
              (setenv "DBUS_FATAL_WARNINGS" "0")
              (zero? (system* "dbus-launch" "ctest" ".")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "QML wrapper for BlueZ")
    (description "bluez-qt is a Qt-style library for accessing the bluez
Bluetooth stack.  It is used by the KDE Bluetooth stack, BlueDevil.")
    (license (list license:lgpl2.1+ license:lgpl3+))))

(define-public breeze-icons
  (package
    (name "breeze-icons")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1dh7bijx99sdb3vn6394wmm5cq0fvvmz8h17sx4hakmbga849cx2"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Default KDE Plasma 5 icon theme")
    (description "Breeze provides a freedesktop.org compatible icon theme.
It is the default icon theme for the KDE Plasma 5 desktop.")
    ;; The license file mentions lgpl3+. The license files in the source
    ;; directories are lgpl3, while the top directory contains the lgpl2.1.
    ;; text.
    (license license:lgpl3+)))

(define-public kapidox
  (package
    (name "kapidox")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "19a7alvn71nxflsyi7y3hghx1iw04qqc77qy54mcxcpkiyvpsggf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; has no test target
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (propagated-inputs
     ;; kapidox is a python programm
     ;; TODO: check if doxygen has to be installed, the readme does not
     ;; mention it. The openSuse .rpm lists doxygen, graphviz, graphviz-gd,
     ;; and python-xml.
     `(("python" ,python)
       ("python-jinja2" ,python-jinja2)
       ("python-pyyaml" ,python-pyyaml)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Doxygen Tools")

    (description "This framework contains scripts and data for building API
documentation (dox) in a standard format and style for KDE.

For the actual documentation extraction and formatting the Doxygen tool is
used, but this framework provides a wrapper script to make generating the
documentation more convenient (including reading settings from the target
framework or other module) and a standard template for the generated
documentation.")
    ;; Most parts are bsd-2, but incuded jquery is expat
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:bsd-2 license:expat))))

(define-public karchive
  (package
    (name "karchive")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1n5nfhrfvqnrdjgjjy7arqik4fya5bp3dvxa16mlhqr19azkavzq"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("bzip2" ,bzip2)
       ("qtbase" ,qtbase)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt 5 addon providing access to numerous types of archives")
    (description "KArchive provides classes for easy reading, creation and
manipulation of 'archive' formats like ZIP and TAR.

It also provides transparent compression and decompression of data, like the
GZip format, via a subclass of QIODevice.")
    ;; The included licenses is are gpl2 and lgpl2.1, but the sources are
    ;; under a variety of licenses.
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:lgpl2.1 license:lgpl2.1+
                   license:lgpl3+ license:bsd-2))))

(define-public kcodecs
  (package
    (name "kcodecs")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1qpzjh3qc2zz80j2bmlinipbispms14k9bmqw8v61zhi6in9z14c"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "String encoding and manipulating library")
    (description "KCodecs provide a collection of methods to manipulate
strings using various encodings.

It can automatically determine the charset of a string, translate XML
entities, validate email addresses, and find encodings by name in a more
tolerant way than QTextCodec (useful e.g. for data coming from the
Internet).")
    ;; The included licenses is are gpl2 and lgpl2.1, but the sources are
    ;; under a variety of licenses.
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:gpl2 license:gpl2+ license:bsd-2
                   license:lgpl2.1 license:lgpl2.1+ license:expat
                   license:lgpl3+ license:mpl1.1))))

(define-public kconfig
  (package
    (name "kconfig")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1dc2i6icyigw1j6qxgdza6j2g8afh390qmxsa2a54mwl84fkfmxv"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("inetutils" ,inetutils)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda* _
              (setenv "HOME" (getcwd))
              (setenv "TMPDIR" (getcwd))
             #t))
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system (string-append (assoc-ref inputs "xorg-server")
                                     "/bin/Xvfb :1 &"))
              (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Kconfiguration settings framework for Qt")
    (description "KConfig provides an advanced configuration system.
It is made of two parts: KConfigCore and KConfigGui.

KConfigCore provides access to the configuration files themselves.
It features:

@enumerate
@item Code generation: describe your configuration in an XML file, and use
`kconfig_compiler to generate classes that read and write configuration
entries.

@item Cascading configuration files (global settings overridden by local
settings).

@item Optional shell expansion support (see docs/options.md).

@item The ability to lock down configuration options (see docs/options.md).
@end enumerate

KConfigGui provides a way to hook widgets to the configuration so that they
are automatically initialized from the configuration and automatically
propagate their changes to their respective configuration files.")
    ;; The included licenses is are gpl2 and lgpl2.1, but the sources are
    ;; under a variety of licenses.
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:lgpl2.1 license:lgpl2.1+ license:expat
                   license:lgpl3+ license:gpl1 ; licende:mit-olif
                   license:bsd-2 license:bsd-3))))

(define-public kcoreaddons
  (package
    (name "kcoreaddons")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "06sx7by3nvaridnavj5p0bxv4nh47n708jlacfw8ydaikmd9i03h"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server))) ; for the tests
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; FIXME: Test failure caused by stout/stderr being interleaved.
       #:phases
        (modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda* _
              (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; enable debug output
              (setenv "HOME" (getcwd))
              (setenv "TMPDIR" (getcwd)))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt addon library with a collection of non-GUI utilities")
    (description "KCoreAddons provides classes built on top of QtCore to
perform various tasks such as manipulating mime types, autosaving files,
creating backup files, generating random sequences, performing text
manipulations such as macro replacement, accessing user information and
many more.")
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kdbusaddons
  (package
    (name "kdbusaddons")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "183nxqrhz4qk4qfp1w4an0scp2dvfqcaqbpg4cgbgk0z590q0pkk"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* _
              (setenv "DBUS_FATAL_WARNINGS" "0")
              (zero? (system* "dbus-launch" "ctest" ".")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Convenience classes for DBus")
    (description "KDBusAddons provides convenience classes on top of QtDBus,
as well as an API to create KDED modules.")
    ;; Some source files mention lgpl2.0+, but the included license is
    ;; the lgpl2.1. Some source files are under non-copyleft licenses.
    (license license:lgpl2.1+)))

(define-public kdnssd
  (package
    (name "kdnssd")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "01b650g031apxc3vd2m91g2fxqk9l8ap67z6rafniphfwy8i0d5m"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Network service discovery using Zeroconf")
    (description "KDNSSD is a library for handling the DNS-based Service
Discovery Protocol (DNS-SD), the layer of Zeroconf that allows network services,
such as printers, to be discovered without any user intervention or centralized
infrastructure.")
    (license license:lgpl2.1+)))

(define-public kguiaddons
  (package
    (name "kguiaddons")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0ig96ah20ybg5rwpswj9va2klvkh2q4amwxmgy3z4niwfsm2g3ic"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda* _
              (setenv "QT_QPA_PLATFORM" "offscreen")
              #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Utilities for graphical user interfaces")
    (description "The KDE GUI addons provide utilities for graphical user
interfaces in the areas of colors, fonts, text, images, keyboard input.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public ki18n
  (package
    (name "ki18n")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0cw24spmwsqa3ppkw03cm6yjd3sfll0dbbk2ya76fd4nw9hb00dv"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("gettext" ,gnu-gettext)
       ("python" ,python)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda* _
              (setenv "HOME" (getcwd)))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Gettext-based UI text internationalization")
    (description "KI18n provides functionality for internationalizing user
interface text in applications, based on the GNU Gettext translation system.  It
wraps the standard Gettext functionality, so that the programmers and translators
can use the familiar Gettext tools and workflows.

KI18n provides additional functionality as well, for both programmers and
translators, which can help to achieve a higher overall quality of source and
translated text.  This includes argument capturing, customizable markup, and
translation scripting.")
    (license license:lgpl2.1+)))

(define-public kidletime
  (package
    (name "kidletime")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "09jsj0pj27h93nr8v46savs6b93h8frydinfr7wlijkvpsl02jb4"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Reporting of idle time of user and system")
    (description "KIdleTime is a singleton reporting information on idle time.
It is useful not only for finding out about the current idle time of the PC,
but also for getting notified upon idle time events, such as custom timeouts,
or user activity.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kitemmodels
  (package
    (name "kitemmodels")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1s1p4nw1pqdzbdwvjnka17p9avf00wadr437p4f96md1lvh3sh69"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system (string-append (assoc-ref inputs "xorg-server")
                                     "/bin/Xvfb :1 &"))
              (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Set of item models extending the Qt model-view framework")
    (description "KItemModels provides the following models:

@enumerate
@item KBreadcrumbSelectionModel - Selects the parents of selected items to
create breadcrumbs.

@item KCheckableProxyModel - Adds a checkable capability to a source model.

@item KConcatenateRowsProxyModel - Concatenates rows from multiple source models.

@item KDescendantsProxyModel - Proxy Model for restructuring a Tree into a list.

@item KExtraColumnsProxyModel - Adds columns after existing columns.

@item KLinkItemSelectionModel - Share a selection in multiple views which do
not have the same source model.

@item KModelIndexProxyMapper - Mapping of indexes and selections through proxy
models.

@item KRearrangeColumnsProxyModel - Can reorder and hide columns from the source
model.

@item KRecursiveFilterProxyModel - Recursive filtering of models.

@item KSelectionProxyModel - A Proxy Model which presents a subset of its source
model to observers
@end enumerate")
    (license license:lgpl2.1+)))

(define-public kitemviews
  (package
    (name "kitemviews")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0y3fx9hk1x27arrmwfzq783a44cs7p8dpmhxrwzh0di4mwa8jafw"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda* _
              (setenv "DBUS_FATAL_WARNINGS" "0")))
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system (string-append (assoc-ref inputs "xorg-server")
                                     "/bin/Xvfb :1 &"))
              (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Set of item views extending the Qt model-view framework")
    (description "KItemViews includes a set of views, which can be used with
item models.  It includes views for categorizing lists and to add search filters
to flat and hierarchical lists.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kplotting
  (package
    (name "kplotting")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0gpypq9kh4b5s6dc7py3m117k3nbxczsfkxgxd9zxvr35kig7ya2"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system (string-append (assoc-ref inputs "xorg-server")
                                     "/bin/Xvfb :1 &"))
              (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Data plotting library")
    (description "KPlotWidget is a QWidget-derived class that provides a virtual
base class for easy data-plotting.  The idea behind KPlotWidget is that you only
have to specify information in \"data units\", the natural units of the
data being plotted.  KPlotWidget automatically converts everything to screen
pixel units.")
    (license license:lgpl2.1+)))

(define-public kwayland
  (package
    (name "kwayland")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1h5anbqrxcl1s8kx1l53vcsfr8ifamcjqd47dk8a7lwr1ga6myq2"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("qtbase" ,qtbase)
       ("wayland" ,wayland)))
    (arguments
     `(#:tests? #f ; FIXME tests require weston to run
                   ; weston requires wayland flags in mesa
       #:phases
         (modify-phases %standard-phases
           (add-before 'check 'check-setup
             (lambda* _
               (setenv "XDG_RUNTIME_DIR" "/tmp"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt-style API to interact with the wayland client and server")
    (description "As the names suggest they implement a Client respectively a
Server API for the Wayland protocol.  The API is Qt-styled removing the needs to
interact with a for a Qt developer uncomfortable low-level C-API.  For example
the callback mechanism from the Wayland API is replaced by signals, data types
are adjusted to be what a Qt developer expects - two arguments of int are
represented by a QPoint or a QSize.")
    (license license:lgpl2.1+)))

(define-public kwidgetsaddons
  (package
    (name "kwidgetsaddons")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1kppx0ppfhnb6q6sijs2dffyar86wkkx8miqavsjsgw1l2wiymcx"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; FIXME: Regression after update to qt 5.7
       #:phases
        (modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda _
              (setenv "QT_QPA_PLATFORM" "offscreen") ; a better solution to Xvfb
              (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; enable debug info
              (setenv "DBUS_FATAL_WARNINGS" "0")
              #t))
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              ;; Xvfb doesn't have proper glx support and needs a pixeldepth
              ;; of 24 bit to avoid "libGL error: failed to load driver: swrast"
              ;;                    "Could not initialize GLX"
              (system (string-append (assoc-ref inputs "xorg-server")
                                     "/bin/Xvfb :1 -screen 0 640x480x24 &"))
              (setenv "DISPLAY" ":1")
              #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Large set of desktop widgets")
    (description "Provided are action classes that can be added to toolbars or
menus, a wide range of widgets for selecting characters, fonts, colors, actions,
dates and times, or MIME types, as well as platform-aware dialogs for
configuration pages, message boxes, and password requests.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kwindowsystem
  (package
    (name "kwindowsystem")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0w5ym8msl80v3q65253pdpj9f1fmb658rnndlbkrgpmm1rv1n6dz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server))) ; for the tests
    (inputs
     `(("libxrender" ,libxrender)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("xcb-utils-keysyms" ,xcb-util-keysyms)))
    (arguments
     `(#:tests? #f)) ; FIXME: 8/10 tests fail.
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE access to the windowing system")
    (description "KWindowSystem provides information about and allows
interaction with the windowing system.  It provides a high level API, which
is windowing system independent and has platform specific
implementations.  This API is inspired by X11 and thus not all functionality
is available on all windowing systems.

In addition to the high level API, this framework also provides several
lower level classes for interaction with the X Windowing System.")
    ;; Some source files mention lgpl2.0+, but the included license is
    ;; the lgpl2.1. Some source files are under non-copyleft licenses.
    (license license:lgpl2.1+)))

(define-public modemmanager-qt
  (package
    (name "modemmanager-qt")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0khz5bf84xxa8aqpzwb6x839xx6dbiadwqhyj7cvgha65fh2xinh"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ; Headers contain #include <ModemManager/ModemManager.h>
     `(("modem-manager", modem-manager)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* _
              (setenv "DBUS_FATAL_WARNINGS" "0")
              (zero? (system* "dbus-launch" "ctest" ".")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Qt wrapper for ModemManager DBus API")
    (description "ModemManagerQt provides access to all ModemManager features
exposed on DBus.  It allows you to manage modem devices and access to
information available for your modem devices, like signal, location and
messages.")
    (license license:lgpl2.1+)))

(define-public networkmanager-qt
  (package
    (name "networkmanager-qt")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "11wy0ds0hqbba900ggkcxjfqc9n65xlzc3h1zv9433nn5d75v6fy"))))
     (build-system cmake-build-system)
     (native-inputs
      `(("extra-cmake-modules" ,extra-cmake-modules)
        ("dbus" ,dbus)
        ("pkg-config" ,pkg-config)))
     (propagated-inputs
      ; Headers contain #include <NetworkManager.h> and
      ;                 #include <libnm/NetworkManager.h>
      `(("network-manager" ,network-manager)))
     (inputs
      `(("qtbase" ,qtbase)))
     (arguments
      `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* _
               (setenv "DBUS_FATAL_WARNINGS" "0")
               (zero? (system* "dbus-launch" "ctest" ".")))))))
     (home-page "https://community.kde.org/Frameworks")
     (synopsis "Qt wrapper for NetworkManager DBus API")
     (description "NetworkManagerQt provides access to all NetworkManager
features exposed on DBus.  It allows you to manage your connections and control
your network devices and also provides a library for parsing connection settings
which are used in DBus communication.")
     (license license:lgpl2.1+)))

(define-public oxygen-icons
  (package
    (name "oxygen-icons")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "5" "-" version ".tar.xz"))
        (sha256
         (base32
          "1c7spjbzk04725vv0ly7vmyvwa96mfa5ki2pm146ld4888a896wm"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen provides the standard icon theme for the KDE desktop")
    (description "Oxygen icon theme for the KDE desktop")
    (license license:lgpl3+)))

(define-public solid
  (package
    (name "solid")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "00wvsxcnvhdx7ijzpcz5wny2ypkxr1drdpr4yvawgpwa678l1107"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("udev" ,eudev)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Desktop hardware abstraction")
    (description "Solid is a device integration framework.  It provides a way of
querying and interacting with hardware independently of the underlying operating
system.")
    (license license:lgpl2.1+)))

(define-public sonnet
  (package
    (name "sonnet")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "152xz7fb1iwhb5w1n4xqvc648iaxi0inrl4kavxcsir61das1xyl"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Multi-language spell checker")
    (description "Sonnet is a plugin-based spell checking library for Qt-based
applications.  It supports several different plugins, including HSpell, Enchant,
ASpell and HUNSPELL.")
    (license license:lgpl2.1+)))

(define-public threadweaver
  (package
    (name "threadweaver")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "02g60zr9cc4bg1p90giich4n0qvqaiakz0y94qrnyj9f7fg0yksl"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Helper for multithreaded programming")
    (description "ThreadWeaver is a helper for multithreaded programming.  It
uses a job-based interface to queue tasks and execute them in an efficient way.")
    (license license:lgpl2.1+)))


;; Tier 2
;;
;; Tier 2 frameworks additionally depend on tier 1 frameworks, but still have
;; easily manageable dependencies.

(define-public kauth
  (package
    (name "kauth")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "14sjjfgl3arqyqcr77w9qhpnd8mrnh53r5rfss6bvlk26bmihs49"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("polkit-qt" ,polkit-qt)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* _
              (setenv "DBUS_FATAL_WARNINGS" "0")
              (zero? (system* "dbus-launch" "ctest" ".")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Execute actions as privileged user")
    (description "KAuth provides a convenient, system-integrated way to offload
actions that need to be performed as a privileged user to small set of helper
utilities.")
    (license license:lgpl2.1+)))

(define-public kcompletion
  (package
    (name "kcompletion")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1qln0v31gn86kzwhnkijr1ydf129n32jmiybbckrp4w6hyx6xfxv"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda _
              (setenv "QT_QPA_PLATFORM" "offscreen")
              #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Powerful autocompletion framework and widgets")
    (description "This framework helps implement autocompletion in Qt-based
applications.  It provides a set of completion-ready widgets, or can be
integrated it into your application's other widgets.")
    (license license:lgpl2.1+)))

(define-public kcrash
  (package
    (name "kcrash")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1lahgfwlp9b5rsl244kzp7rsl4ybv1q4qlvpv0xxz5ygssk48l0w"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system (string-append (assoc-ref inputs "xorg-server")
                                     "/bin/Xvfb :1 &"))
              (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Graceful handling of application crashes")
    (description "KCrash provides support for intercepting and handling
application crashes.")
    (license license:lgpl2.1+)))

(define-public kdoctools
  (package
    (name "kdoctools")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1r129kpq0d11b9l87cqbal6fm5ycwhsps1g3r1a7jsxz70scz4ri"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("karchive" ,karchive)
       ("ki18n" ,ki18n)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("perl-uri" ,perl-uri)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'cmake-find-docbook
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "cmake" "\\.cmake$")
                (("CMAKE_SYSTEM_PREFIX_PATH")
                  "CMAKE_PREFIX_PATH"))
               (substitute* "cmake/FindDocBookXML4.cmake"
                 (("^.*xml/docbook/schema/dtd.*$")
                   "xml/dtd/docbook\n"))
               (substitute* "cmake/FindDocBookXSL.cmake"
                 (("^.*xml/docbook/stylesheet.*$")
                  (string-append "xml/xsl/docbook-xsl-"
                                 ,(package-version docbook-xsl) "\n"))))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Create documentation from DocBook")
    (description "Provides tools to generate documentation in various format
from DocBook files.")
    (license license:lgpl2.1+)))

(define-public kfilemetadata
  (package
    (name "kfilemetadata")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "02n9qhpr0jlwdgdbid0k34abhs3bzhlsa56ybl5dq1aib6izk1sy"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("python-2" ,python-2)))
    (inputs
     `(("attr" ,attr)
       ("karchive" ,karchive)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Extract metadata from different fileformats")
    (description "KFileMetaData provides a simple library for extracting the
text and metadata from a number of different files.  This library is typically
used by file indexers to retreive the metadata.  This library can also be used
by applications to write metadata.")
    (license (list license:lgpl2.0 license:lgpl2.1 license:lgpl3))))

(define-public kimageformats
  (package
    (name "kimageformats")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "12mhgckmhnvcnm8k7mk15mipxrnm7i9ip7ykbjh8nxjiwyk1pmwc"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 &"))
             (setenv "DISPLAY" ":1")
            #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Plugins to allow QImage to support extra file formats")
    (description "This framework provides additional image format plugins for
QtGui.  As such it is not required for the compilation of any other software,
but may be a runtime requirement for Qt-based software to support certain image
formats.")
    (license license:lgpl2.1+)))

(define-public kjobwidgets
  (package
    (name "kjobwidgets")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1mcvrz66xcqjgbp08zpqsf943cm462wbqm5gh719p9s25hx8hwrc"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Widgets for showing progress of asynchronous jobs")
    (description "KJobWIdgets provides widgets for showing progress of
asynchronous jobs.")
    (license license:lgpl2.1+)))

(define-public knotifications
  (package
    (name "knotifications")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0qryp41phnpx4r9wa6rfhmnzy7nxl0ijnyrafadf2n2xb53ipkpa"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("qttools" ,qttools)))
    (inputs
     `(("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda* _
             (setenv "HOME" (getcwd))))
         (replace 'check
           (lambda* _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (zero? (system* "dbus-launch" "ctest" ".")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Desktop notifications")
    (description "KNotification is used to notify the user of an event.  It
covers feedback and persistent events.")
    (license license:lgpl2.1+)))

(define-public kpackage
  (package
    (name "kpackage")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "03aqzkpqz3c1v4qgwfbs3ncdbapiyg7psrkhxqv3z48rklavk1ri"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; FIXME: 1/4 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda* _
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; enable debug output
             (setenv "HOME" (getcwd)))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Installation and loading of additional content as packages")
    (description "The Package framework lets the user install and load packages
of non binary content such as scripted extensions or graphic assets, as if they
were traditional plugins.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kpty
  (package
    (name "kpty")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1ybvdzqpa53kkki9p5da0ff9x3c63rmksk7865wqwlgy8apzi2fs"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; FIXME: 1/1 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
         (lambda _
           (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
           (substitute* "autotests/kptyprocesstest.cpp"
             (("/bin/bash") (which "bash")))
           #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Interfacing with pseudo terminal devices")
    (description "This library provides primitives to interface with pseudo
terminal devices as well as a KProcess derived class for running child processes
and communicating with them using a pty.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kunitconversion
  (package
    (name "kunitconversion")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "03dfjn4lm6sl2zcdrvw0b9irzvkyc2w2j5xixag5j8nw373742h8"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Converting physical units")
    (description "KUnitConversion provides functions to convert values in
different physical units.  It supports converting different prefixes (e.g. kilo,
mega, giga) as well as converting between different unit systems (e.g. liters,
gallons).")
    (license license:lgpl2.1+)))


;; Tier 3
;;
;; Tier 3 frameworks are generally more powerful, comprehensive packages, and
;; consequently have more complex dependencies.

(define-public baloo
  (package
    (name "baloo")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1ayfdg6j9lvas17ryjdv4a0kaj6vw3bxfy2x9nadl0gkc9pak4nh"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kfilemetadata" ,kfilemetadata)))
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kbookmarks" ,kbookmarks)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kidletime" ,kidletime)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("lmdb" ,lmdb)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t))
         (replace 'check
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (zero? (system* "dbus-launch" "ctest" ".")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "File searching and indexing")
    (description "Baloo provides file searching and indexing.  It does so by
maintaining an index of the contents of your files.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kactivities
  (package
    (name "kactivities")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0s8g43zk6h35bq1am1nnhj0qvmhd6kz42gs8l7ybga0367jghzhf"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("boost" ,boost)
       ("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Core components for the KDE Activity concept")
    (description "KActivities provides the infrastructure needed to manage a
user's activites, allowing them to switch between tasks, and for applications
to update their state to match the user's current activity.  This includes a
daemon, a library for interacting with that daemon, and plugins for integration
with other frameworks.")
    ;; triple licensed
    (license (list license:gpl2+ license:lgpl2.0+ license:lgpl2.1+))))

;; NOTE: This package is listed as a tier 2 package even though it requires
;;       kactivities - a tier 3 package.
(define-public kactivities-stats
  (package
    (name "kactivities-stats")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1z3xvpifxbd05b2xaxxyiypcpid7jgjb1qpwiyjj1gnfp4rjmzpc"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("boost" ,boost)
       ("kactivities" ,kactivities)
       ("kconfig" ,kconfig)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Access usage statistics collected by the activity manager")
    (description "The KActivitiesStats library provides a querying mechanism for
the data that the activitiy manager collects - which documents have been opened
by which applications, and what documents have been linked to which activity.")
    ;; triple licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+ license:lgpl3+))))

(define-public kbookmarks
  (package
    (name "kbookmarks")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "10d8dnhvbrwp0dbmz93cqfdff6ir8iy3yiwaf9ihj6ma124qlyjn"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kwidgetsaddons" ,kwidgetsaddons)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Bookmarks management library")
    (description "KBookmarks lets you access and manipulate bookmarks stored
using the XBEL format.")
    (license license:lgpl2.1+)))

(define-public kcmutils
  (package
    (name "kcmutils")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0aws1c76s6wbp0xpr6qv6cfwq8dw82v00pkf9gy84sbxknwjnizk"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kconfigwidgets" ,kconfigwidgets)
       ("kservice" ,kservice)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kpackage" ,kpackage)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Utilities for KDE System Settings modules")
    (description "KCMUtils provides various classes to work with KCModules.
KCModules can be created with the KConfigWidgets framework.")
    (license license:lgpl2.1+)))

(define-public kconfigwidgets
  (package
    (name "kconfigwidgets")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0v25r50gh5i984lzlv0rradghglcfqf0gsfmnkn23h87b86fm9l2"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kwidgetsaddons" ,kwidgetsaddons)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Widgets for configuration dialogs")
    (description "KConfigWidgets provides easy-to-use classes to create
configuration dialogs, as well as a set of widgets which uses KConfig to store
their settings.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kdeclarative
  (package
    (name "kdeclarative")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "00ik9q1r6y6g5rkdq96yczgrxmcg85x00lipyljvc3x6xw6bixbz"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kconfig" ,kconfig)
       ("kpackage" ,kpackage)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kglobalaccel" ,kglobalaccel)
       ("kguiaddons" ,kguiaddons)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 -screen 0 640x480x24 &"))
             (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Integration of QML and KDE work spaces")
    (description "KDeclarative provides integration of QML and KDE work spaces.
It's comprises two parts: a library used by the C++ part of your application to
intergrate QML with KDE Frameworks specific features, and a series of QML imports
that offer bindings to some of the Frameworks.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kded
  (package
    (name "kded")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0ngpxdxb596myn5r4kjxahx195bwklq33yvgjvcbxi2clg2wccaj"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kinit" ,kinit)
       ("kservice" ,kservice)
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Central daemon of KDE work spaces")
    (description "KDED stands for KDE Daemon.  KDED runs in the background and
performs a number of small tasks.  Some of these tasks are built in, others are
started on demand.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kdesignerplugin
  (package
    (name "kdesignerplugin")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0i0s8pwwhwh5hyyvkv0cnj0yyv0g5bnm5xw18knv2yagiy4bvb2j"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdoctools" ,kdoctools)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Integrating KDE frameworks widgets with Qt Designer")
    (description "This framework provides plugins for Qt Designer that allow it
to display the widgets provided by various KDE frameworks, as well as a utility
(kgendesignerplugin) that can be used to generate other such plugins from
ini-style description files.")
    (license license:lgpl2.1+)))

(define-public kdesu
  (package
    (name "kdesu")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1ivcnhgvq75xvl0w9g7m45qzallz42ijaq0n1ap09lpdfmjbnrxk"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kpty" ,kpty)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kservice" ,kservice)
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "User interface for running shell commands with root privileges")
    (description "KDESU provides functionality for building GUI front ends for
(password asking) console mode programs.  kdesu and kdessh use it to interface
with su and ssh respectively.")
    (license license:lgpl2.1+)))

(define-public kemoticons
  (package
    (name "kemoticons")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0gmc52k5jb553jvzxwsq79v5y87kgav8i5qqv4bqc9yl7p866zhn"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kservice" ,kservice)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; FIXME: 2/2 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; Enable debug output
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Convert text emoticons to graphical emoticons")
    (description "KEmoticons converts emoticons from text to a graphical
representation with images in HTML.  It supports setting different themes for
emoticons coming from different providers.")
    ;; dual licensed, image files are licensed under cc-by-sa4.0
    (license (list license:gpl2+ license:lgpl2.1+ license:cc-by-sa4.0))))

(define-public kglobalaccel
  (package
    (name "kglobalaccel")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "123v0ld1q88hbm3d0mqgq6lcivfkqh7pbz4hb4n76ab5v43qc15c"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("libxcb" ,libxcb)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Global desktop keyboard shortcuts")
    (description "KGlobalAccel allows you to have global accelerators that are
independent of the focused window.  Unlike regular shortcuts, the application's
window does not need focus for them to be activated.")
    (license license:lgpl2.1+)))

(define-public kiconthemes
  (package
    (name "kiconthemes")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1k5zig2n6wzfyv6pc8dpas2862mxjyxxza00m31myrfw5i1a1h6m"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kcoreaddons" ,kcoreaddons)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("ki18n" ,ki18n)
       ("kitemviews" ,kitemviews)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (arguments
     `(#:tests? #f ; FIXME: Test failure
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "XDG_DATA_DIRS"
                     (string-append (assoc-ref inputs "shared-mime-info")
                                    "/share"))
             (setenv "HOME" (getcwd))
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; Enable debug output
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Icon GUI utilities")
    (description "This library contains classes to improve the handling of icons
in applications using the KDE Frameworks.")
    (license license:lgpl2.1+)))

(define-public kinit
  (package
    (name "kinit")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1i7l6gid5hrrfglw1c461gpjg51dwz7cl4lx7ll8vz2ha8mz4d3n"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Library to speed up start of applications on KDE workspaces")
    (description "Kdeinit is a process launcher similar to init used for booting
UNIX.  It launches processes by forking and then loading a dynamic library which
contains a 'kdemain(...)' function.  Using kdeinit to launch KDE applications
makes starting KDE applications faster and reduces memory consumption.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kio
  (package
    (name "kio")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0zncj9yf8zaylazlwvirylpk9vki3j889b1x2s0aav54vvj7vdi5"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kbookmarks" ,kbookmarks)
       ("kconfig" ,kconfig)
       ("kcompletion" ,kcompletion)
       ("kcoreaddons" ,kcoreaddons)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kxmlgui" ,kxmlgui)
       ("solid" ,solid)))
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("acl" ,acl)
       ("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kiconthemes" ,kiconthemes)
       ("ki18n" ,ki18n)
       ("knotifications" ,knotifications)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f ; FIXME: 41/50 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    ;;(replace 'check
    ;;  (lambda _
    ;;    (setenv "DBUS_FATAL_WARNINGS" "0")
    ;;    (zero? (system* "dbus-launch" "ctest" ".")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Network transparent access to files and data")
    (description "This framework implements a lot of file management functions.
It supports accessing files locally as well as via HTTP and FTP out of the box
and can be extended by plugins to support other protocols as well.  There is a
variety of plugins available, e.g. to support access via SSH.  The framework can
also be used to bridge a native protocol to a file-based interface.  This makes
the data accessible in all applications using the KDE file dialog or any other
KIO enabled infrastructure.")
    (license license:lgpl2.1+)))

(define-public knewstuff
  (package
    (name "knewstuff")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0xdv3wh3100vzsx8p2zihy1dvh0wzfmrjkjq71v8igwz5d291zsj"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("attica" ,attica)
       ("kservice" ,kservice)
       ("kxmlgui" ,kxmlgui)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kjobwidgets" ,kjobwidgets)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("solid" ,solid)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f ; FIXME: 1/3 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _ ; XDG_DATA_DIRS isn't set
             (setenv "HOME" (getcwd))
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Framework for downloading and sharing additional application data")
    (description "The KNewStuff library implements collaborative data sharing
for applications.  It uses libattica to support the Open Collaboration Services
specification.")
    (license license:lgpl2.1+)))

(define-public knotifyconfig
  (package
    (name "knotifyconfig")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1dij841fnqia4p44x2wnpdvl8cn3nkj833y0fah50fmipjc8r70b"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("knotifications" ,knotifications)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Configuration dialog for desktop notifications")
    (description "KNotifyConfig provides a configuration dialog for desktop
notifications which can be embedded in your application.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kparts
  (package
    (name "kparts")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0z7qr93aq02i7g7cxgypx2rzlnsvbsx9cjblb0ijmad1nb8w3mix"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kio" ,kio)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("solid" ,solid)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Plugin framework for user interface components")
    (description "This library implements the framework for KDE parts, which are
widgets with a user-interface defined in terms of actions.")
    (license license:lgpl2.1+)))

(define-public kpeople
  (package
    (name "kpeople")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0iknzkj23y927xh24kw5sjxyirhy6pkmfcmmgwzd78rba8a54qp2"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (arguments
     `(#:tests? #f ; FIXME: 1/3 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; Enable debug output
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Provides access to all contacts and aggregates them by person")
    (description "KPeople offers unified access to our contacts from different
sources, grouping them by person while still exposing all the data.  KPeople
also provides facilities to integrate the data provided in user interfaces by
providing QML and Qt Widgets components.  The sources are plugin-based, allowing
to easily extend the contacts collection.")
    (license license:lgpl2.1+)))

(define-public krunner
  (package
    (name "krunner")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0ff87ijjd47jxf6zw2ggqgngnbyx1rj59wdfgy5wbi3acws6bafl"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("plasma-framework" ,plasma-framework)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kpackage" ,kpackage)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)
       ("threadweaver" ,threadweaver)))
    (arguments
     `(#:tests? #f ; FIXME: 1/1 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; Enable debug output
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Framework for Plasma runners")
    (description "The Plasma workspace provides an application called KRunner
which, among other things, allows one to type into a text area which causes
various actions and information that match the text appear as the text is being
typed.")
    (license license:lgpl2.1+)))

(define-public kservice
  (package
    (name "kservice")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0w0nsg64d6xhgijr2vh0j5p544qi0q55jpqa9v9mv956zrrdssdk"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; FIXME: 8/10 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Plugin framework for desktop services")
    (description "KService provides a plugin framework for handling desktop
services.  Services can be applications or libraries.  They can be bound to MIME
types or handled by application specific code.")
    ;; triple licensed
    (license (list license:gpl2+ license:gpl3+ license:lgpl2.1+))))

(define-public ktexteditor
  (package
    (name "ktexteditor")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1ykj1kvm7k1vxb1w235d5hp2swwdqjyp2y4c3pxbvkn999h9x5q5"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kparts" ,kparts)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kguiaddons" ,kguiaddons)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("libgit2" ,libgit2)
       ("perl" ,perl)
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("solid" ,solid)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f ; FIXME: 2/54 tests fail: Cannot find fontdirectory qtbase/lib/font
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "XDG_DATA_DIRS" ; FIXME build phase doesn't find parts.desktop
                     (string-append (assoc-ref inputs "kparts") "/share"))
             #t))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Full text editor component")
    (description "KTextEditor provides a powerful text editor component that you
can embed in your application, either as a KPart or using the KF5::TextEditor
library.")
    ;; triple licensed
    (license (list license:gpl2+ license:lgpl2.0+ license:lgpl2.1+))))

(define-public ktextwidgets
  (package
    (name "ktextwidgets")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1q10xav2gkii6s3m31c9xvxf1988l7k2lpib6pyhgsidflmwjm02"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("ki18n" ,ki18n)
       ("sonnet" ,sonnet)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Text editing widgets")
    (description "KTextWidgets provides widgets for displaying and editing text.
It supports rich text as well as plain text.")
    ;; dual licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public kwallet
  (package
    (name "kwallet")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0zad5h4vsvcl2xv3vxsjwh42b71xbp6x6rj8cvmw8szr2rzz9gsx"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("gpgmepp" ,gpgmepp)
       ("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kiconthemes" ,kiconthemes)
       ("ki18n" ,ki18n)
       ("knotifications" ,knotifications)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("libgcrypt" ,libgcrypt)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Safe desktop-wide storage for passwords")
    (description "This framework contains an interface to KWallet, a safe
desktop-wide storage for passwords and the kwalletd daemon used to safely store
the passwords on KDE work spaces.")
    (license license:lgpl2.1+)))

(define-public kxmlgui
  (package
    (name "kxmlgui")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1qhixldhhcbklmrpjh67440h1rrzqy70h57hw6ialjdsr3pl6ihp"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("attica" ,attica)
       ("kauth", kauth)
       ("kcodecs" ,kcodecs)
       ("kcoreaddons" ,kcoreaddons)
       ("kglobalaccel" ,kglobalaccel)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f ; FIXME: 1/5 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Framework for managing menu and toolbar actions")
    (description "KXMLGUI provides a framework for managing menu and toolbar
actions in an abstract way.  The actions are configured through a XML description
and hooks in the application code.  The framework supports merging of multiple
descriptions for integrating actions from plugins.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kxmlrpcclient
  (package
    (name "kxmlrpcclient")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "06ap6ipzqimz1rfrcr7z8zc7idy7sg4a97dws7h52i34ms7jqnc8"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kio" ,kio)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kitemviews" ,kitemviews)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "XML-RPC client")
    (description "This library contains simple XML-RPC Client support.  It is a
complete client and is easy to use.  Only one interface is exposed,
kxmlrpcclient/client.h and from that interface, you only need to use 3 methods:
setUrl, setUserAgent and call.")
    ;; dual licensed
    (license (list license:bsd-2 license:lgpl2.1+))))

(define-public plasma-framework
  (package
    (name "plasma-framework")
    (version "5.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/frameworks/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0981vm00541dzihlr1fsax05biwp2ddpwjrmvnfysx5jagdc65cb"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("kpackage" ,kpackage)
       ("kservice" ,kservice)))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kactivities" ,kactivities)
       ("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kdoctools" ,kdoctools)
       ("kglobalaccel" ,kglobalaccel)
       ("kguiaddons" ,kguiaddons)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("kio" ,kio)
       ("ki18n" ,ki18n)
       ("kjobwidgets" ,kjobwidgets)
       ("knotificantions" ,knotifications)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)))
    (arguments
     `(#:tests? #f ; FIXME: 13/14 tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; Enable debug output
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Libraries, components and tools of Plasma workspaces")
    (description "The plasma framework provides QML components, libplasma and
script engines.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))
