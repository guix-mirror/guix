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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
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

@itemize
@item Code generation: describe your configuration in an XML file, and use
`kconfig_compiler to generate classes that read and write configuration
entries.

@item Cascading configuration files (global settings overridden by local
settings).

@item Optional shell expansion support (see docs/options.md).

@item The ability to lock down configuration options (see docs/options.md).
@end itemize

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
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system (string-append (assoc-ref inputs "xorg-server")
                                     "/bin/Xvfb :1 &"))
              (setenv "DISPLAY" ":1")
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

@itemize
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
@end itemize")
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
     `(#:tests? #f ; FIXME: libGL error: failed to load driver: swrast.
       #:phases
        (modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda* _
              (setenv "CTEST_OUTPUT_ON_FAILURE" "1") ; enable debug output
              (setenv "LIBGL_DEBUG" "verbose") ; enable debug output
              (setenv "DBUS_FATAL_WARNINGS" "0")))
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system (string-append (assoc-ref inputs "xorg-server")
                                     "/bin/Xvfb :1 &"))
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
    (inputs
     `(("modem-manager", modem-manager)
       ("qtbase" ,qtbase)))
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
     (inputs
      `(("network-manager", network-manager)
        ("qtbase" ,qtbase)))
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
