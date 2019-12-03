;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages kde-utils)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages qt))

(define-public kate
  (package
    (name "kate")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kate-" version ".tar.xz"))
       (sha256
        (base32 "0wgcw10c4grkmsyp79ashwgpy59lgrinwdib4mjclpw2grp0g7xb"))))
    (properties `((tags . ("Desktop" "KDE" "Utilities"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kactivities" ,kactivities)
       ("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kitemmodels" ,kitemmodels)
       ("threadweaver" ,threadweaver)
       ("knewstuff" ,knewstuff)
       ("kio" ,kio)
       ("kjobwidgets" ,kjobwidgets)
       ("kparts" ,kparts)
       ("ktexteditor" ,ktexteditor)
       ("kwallet" ,kwallet)
       ("plasma-framework" ,plasma-framework)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)))
    (arguments
     `(#:tests? #f ;; 2/7 tests fail
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "XDG_CACHE_HOME" "/tmp/xdg-cache")
             #t)))))
    (home-page "https://kate-editor.org/")
    (synopsis "Multi-document, multi-view text editor")
    (description "Kate is a powerful text editor that can open multiple files
simultaneously.

With a built-in terminal, syntax highlighting, and tabbed sidebar, it performs
as a lightweight but capable development environment.  Kate's many tools,
plugins, and scripts make it highly customizable.

Kate's features include:
@itemize
@item Multiple saved sessions, each with numerous files
@item Scriptable syntax highlighting, indentation, and code-folding
@item Configurable templates and text snippets
@item Symbol viewers for C, C++, and Python
@item XML completion and validation
@end itemize")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0))))

(define-public kmag
  (package
    (name "kmag")
    (version "19.08.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/applications/" version
                          "/src/kmag-" version ".tar.xz"))
      (sha256
       (base32 "0l69mgnh2mmkxawwibqdx9n7myl6qqnr2fd3mpsg2bzpcfvmsvi1"))))
    (properties `((tags . ("Desktop" "KDE" "Utilities"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ;; TODO: QAccessibilityClient - libqaccessibilityclien
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/utilities/org.kde.kmag")
    (synopsis "Screen magnifier tool")
    (description "You can use KMagnifier to magnify a part of the screen just
as you would use a lens to magnify a newspaper fine-print or a photograph.
This application is useful for a variety of people: from researchers to
artists to web-designers to people with low vision.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))
