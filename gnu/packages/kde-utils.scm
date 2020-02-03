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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages xorg))

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

(define-public kmousetool
  (package
    (name "kmousetool")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kmousetool-" version ".tar.xz"))
       (sha256
        (base32 "169kk20mkm29nycg2vs1k5cs22gzchqs9hbfd661cy2l7n4d8d04"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kauth" ,kauth)
       ("kcoreaddons" ,kcoreaddons)
       ("kconfigwidgets", kconfigwidgets)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("knotifications" ,knotifications)
       ("kxmlgui" ,kxmlgui)
       ("kwindowsystem" ,kwindowsystem)
       ("libxtst" ,libxtst)
       ("libxt" ,libxt)
       ("phonon" ,phonon)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/utilities/org.kde.kmousetool")
    (synopsis "Automatic mouse click and mouse manipulation tool for the
disabled")
    (description "KMouseTool clicks the mouse whenever the mouse cursor pauses
briefly.  It was designed to help those with repetitive strain injuries, for
whom pressing buttons hurts.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kmouth
  (package
    (name "kmouth")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kmouth-" version ".tar.xz"))
       (sha256
        (base32 "1agjxf1jfi967hj1iz788n6cna6fr7qg80zsx6s119hg7b0xwqmr"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kcompletion" ,kcompletion)
       ("kconfig", kconfig)
       ("kconfigwidgets", kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtspeech" ,qtspeech)))
    (home-page "https://kde.org/applications/utilities/org.kde.kmouth")
    (synopsis "Type-and-say frontend for speech synthesizers")
    (description "KMouth is a program which enables persons that cannot speak
to let their computer speak, e.g. mutal people or people who have lost their
voice.  It has a text input field and speaks the sentences that you enter.  It
also has support for user defined phrasebooks.

It includes a history of spoken sentences from which the user can select
sentences to be re-spoken.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kronometer
  (package
    (name "kronometer")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kronometer/" version
                           "/src/kronometer-" version ".tar.xz"))
       (sha256
        (base32 "05hs8729a3aqjpwmn2xdf2sriacrll4sj4ax3lm4s1ravj09n9bm"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kauth" ,kauth)
       ("kconfig" ,kconfig)
       ("kconfigwidgets", kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/utilities/org.kde.kronometer")
    (synopsis "Simple stopwatch application")
    (description "Kronometer is a stopwatch application.  It features the
basic stopwatch actions (pause, resume, reset, laps), as well as the ability
to save the times and resume them later.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     license:gpl2+)))

(define-public krusader
  (package
    (name "krusader")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/krusader/" version
                           "/krusader-" version ".tar.xz"))
       (sha256
        (base32 "02b1jz5a7cjr13v6c7fczrhs1xmg1krnva5fxk8x2bf4nd1rm8s1"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("karchive" ,karchive)
       ("kbookmarks" ,kbookmarks)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("knotifications" ,knotifications)
       ("kparts" ,kparts)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("solid" ,solid)
       ("zlib" ,zlib)))
    (home-page "https://www.krusader.org")
    (synopsis "Twin-panel (commander-style) file manager")
    (description "Krusader is a simple, easy, yet powerful,
twin-panel (commander-style) file manager, similar to Midnight Commander or
Total Commander

It provides all the file management features you could possibly want.  Plus:
extensive archive handling, mounted filesystem support, FTP, advanced search
module, an internal viewer/editor, directory synchronisation, file content
comparisons, powerful batch renaming and much much more.  It supports a wide
variety of archive formats and can handle other KIO slaves such as smb or
fish.

Almost completely customizable, Krusader is very user friendly, fast and looks
great on your desktop.")
    (license license:gpl2+)))

(define-public okteta
  (package
    (name "okteta")
    (version "17.12.3")
    (source
     (origin
       (method url-fetch)
       ;; TODO: Why is this not in "stable" anymore
       (uri (string-append "mirror://kde/Attic/applications/" version
                           "/src/okteta-" version ".tar.xz"))
       (sha256
        (base32 "03wsv83l1cay2dpcsksad124wzan7kh8zxdw1h0yicn398kdbck4"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("qttools" ,qttools)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("kbookmarks" ,kbookmarks)
       ("kcmutils" ,kcmutils)
       ("kcodecs" ,kcodecs)
       ("kcrash" ,kcrash)
       ("kcompletion" ,kcompletion)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("kparts" ,kparts)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qca" ,qca)
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "HOME" "/tmp/dummy-home")
             #t)))))
    (home-page "https://kde.org/applications/utilities/org.kde.okteta")
    (synopsis "Hexadecimal editor for binary files")
    (description "Okteta is a simple editor for the raw data of files.  This
type of program is also called hex editor or binary editor.

The data is displayed in the traditional view with two columns: one with the
numeric values and one with the assigned characters.  Editing can be done both
in the value column and the character column.  Besides the usual editing
capabilities Okteta also brings a small set of tools, like a table listing
decodings into common simple data types, a table listing all possible bytes
with its character and value equivalents, a info view with a statistic and a
filter tool.  All modifications to the data loaded can be endlessly undone or
redone.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public rsibreak
  (package
    (name "rsibreak")
    (version "0.12.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/rsibreak/0.12/"
                           "rsibreak-" version ".tar.xz"))
       (sha256
        (base32 "09axg6gbmpnxsk88mdjbxxvfaj5af7xaf1gmnr17b0506zcfgwhv"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kauth" ,kauth)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidletime" ,kidletime)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("ktextwidgets" ,ktextwidgets)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/utilities/org.kde.rsibreak")
    (synopsis "Assists in the Recovery and Prevention of Repetitive Strain
Injury")
    (description "Repetitive Strain Injury is an illness which can occur as a
result of working with a mouse and keyboard.  This utility can be used to
remind you to take a break now and then.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public smb4k
  (package
    (name "smb4k")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/smb4k/files/"
                           version "/smb4k-" version ".tar.xz/download"))
       (sha256
        (base32 "1daajaj8qhxkzz8dsaracwi49z4i57466h6qnqnh2ir2l54q00ir"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kauth" ,kauth)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcompletion" ,kcompletion)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kjobwidgets" ,kjobwidgets)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libsmbclient" ,samba)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)))
    (home-page "https://kde.org/applications/utilities/org.kde.smb4k")
    (synopsis "Samba (SMB) share advanced browser")
    (description "Smb4K is an network neighborhood browser for the KDE
Software Compilation and a frontend to the programs of the Samba software
suite.

Features:
@itemize
@item Scanning for (active) workgroups, hosts, and shares
@item Support of the CIFS (Linux) and SMBFS (FreeBSD) file system
@item Mounting and unmounting of shares (using the KAuth framework)
@item Access to the files of a mounted share using a file manager or terminal
@item Auto-detection of external mounts and unmounts
@item Remounting of previously used shares on program start
@item Miscellaneous infos about remote network items and mounted shares
@item Network search
@item WINS server support
@item Preview of the contents of a share
@item Several methods to look up the initial list of workgroups and domains
@item Default login
@item Special handling of homes shares
@item Ability to bookmark favorite shares and organize them in groups
@item System tray widget
@item Support of advanced Samba options
@item Support of printer shares
@item KWallet support
@item Synchronization of a remote share with a local copy and vice versa
@item Ability to define custom options for individual servers and shares
@item Laptop support through the Solid hardware device framework
@end itemize")
    (license license:gpl2+)))

(define-public sweeper
  (package
    (name "sweeper")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/sweeper-" version ".tar.xz"))
       (sha256
        (base32 "1gn87yxmhi7rs82jq7y89bvlx33xbl9wq8kr96pcz423khqvjl84"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kactivities-stats" ,kactivities-stats)
       ("kbookmarks" ,kbookmarks)
       ("kcrash" ,kcrash)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/utilities/org.kde.sweeper")
    (synopsis "Temporary file and history cleaner")
    (description "
Sweeper helps to clean unwanted traces the user leaves on the system and to
regain disk space removing unused temporary files.
It can quickly remove temporary information, such as web page cookies,
browser history, or the list of recently-opened documents.  It helps provide
additional privacy on a system shared between multiple users.")
    (license license:lgpl2.0+ )))
