;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages kde-systemtools)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages search)
  #:use-module (gnu packages xml))

(define-public dolphin
  (package
    (name "dolphin")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/dolphin-" version ".tar.xz"))
       (sha256
        (base32 "0klxyvcj1bmzpsyahj9kq3smvwzww30pjk5c90j6jpf14hizawfy"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("ruby" ,ruby)
       ("ruby-test-unit" ,ruby-test-unit)))
    (inputs
     `(("baloo" ,baloo)
       ("baloo-widgets" ,baloo-widgets)
       ("kactivities" ,kactivities)
       ("kbookmarks" ,kbookmarks)
       ("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("kparts" ,kparts)
       ("ktextwidgets" ,ktextwidgets)
       ("kwindowsystem" ,kwindowsystem)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
    (arguments
     `(#:tests? #f)) ;; TODO: 4/15 tests fail even with offscreen
    (home-page "https://kde.org/applications/system/org.kde.dolphin")
    (synopsis "File manager for KDE")
    (description "Dolphin is a file manager for KDE focusing on usability.
The main features of Dolphin are:
@itemize
@item Navigation bar for URLs, which allows to navigate quickly
      through the file hierarchy.
@item View properties are remembered for each folder.
@item Split of views is supported.
@item Network transparency.
@item Undo/redo functionality.
@item Renaming of a variable number of selected items in one step.
@end itemize")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public dolphin-plugins
  (package
    (name "dolphin-plugins")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/dolphin-plugins-" version ".tar.xz"))
       (sha256
        (base32 "0m9sl5fybk60h7r91a5qfxvwzksg2kxn1bc2ygrr8klm2pv0x1l2"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("dolphin" ,dolphin)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("ktexteditor" ,ktexteditor)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "http://www.kde.org/")
    (synopsis "VCS-Plugins for Dolphin")
    (description "This package contains plugins that offer integration in
Dolphin with the version control systems: Bzr, Git, Mercurial, Subversion.")
    (license license:gpl2+)))

(define-public khelpcenter
  (package
    (name "khelpcenter")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/khelpcenter-" version ".tar.xz"))
       (sha256
        (base32 "0ympq1qm5h14mw18wry7l02ndg1f5kddwkf5bliip6vk2vxiff50"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("grantlee" ,grantlee)
       ("karchive" ,karchive)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("khtml" ,khtml)
       ("ki18n" ,ki18n)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("kjs" ,kjs)
       ("kparts" ,kparts)
       ("kservice" ,kservice)
       ("kwindowsystem" ,kwindowsystem)
       ("libxml2" ,libxml2)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("xapian" ,xapian)))
    (arguments
     `(#:tests? #f)) ;; 1/1 test fails
    (home-page "https://kde.org/applications/system/org.kde.Help")
    (synopsis "KDE documentation viewer")
    (description "KDE documentation viewer")
    (license license:gpl2+)))

(define-public konsole
  (package
    (name "konsole")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/konsole-" version ".tar.xz"))
       (sha256
        (base32 "09bhgqjnqlpxkkgdpn35pvj747ab7waz10zalvpwdpgqkw811iic"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kbookmarks" ,kbookmarks)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("kglobalaccel" ,kglobalaccel)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kparts" ,kparts)
       ("kpty" ,kpty)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)))
    (arguments
     `(#:tests? #f)) ;; TODO: 2/15 tests fail even with HOME, offscreen, SHELL, debus
    (home-page "http://www.kde.org/")
    (synopsis "Terminal emulator similar for KDE")
    (description "Konsole is a terminal emulator, similar to xterm, built on
the KDE Platform.  It can contain multiple terminal sessions inside one window
using detachable tabs.  Konsole supports customizable schemes, saved sessions,
output monitoring and more.

This package is part of the KDE base applications module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))
