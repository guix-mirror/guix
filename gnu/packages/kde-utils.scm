;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages xorg))

(define-public ark
  (package
    (name "ark")
    (version "20.04.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/ark-" version ".tar.xz"))
              (sha256
               (base32
                "0g5bfa1lc7mhrc2ngd4ldf33dpwr7gqrj95kp897pf632wwj23iw"))
              ;; The libarchive package in Guix does not support
              ;; xar; disable related tests.
              (patches (search-patches "ark-skip-xar-test.patch"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xserver
           ;; adddialogtest requires DISPLAY.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server")))
               (setenv "HOME" (getcwd))
               (system (format #f "~a/bin/Xvfb :1 &" xorg-server))
               (setenv "DISPLAY" ":1"))))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lrzip (assoc-ref inputs "lrzip"))
                    (lzop  (assoc-ref inputs "lzop"))
                    (p7zip (assoc-ref inputs "p7zip"))
                    (unzip (assoc-ref inputs "unzip"))
                    (zip   (assoc-ref inputs "zip"))
                    (zstd  (assoc-ref inputs "zstd")))
               (wrap-program (string-append out "/bin/ark")
                 `("PATH" suffix
                   ,(map (lambda (p)
                           (string-append p "/bin"))
                         (list lrzip lzop p7zip unzip zip zstd))))))))))
    (native-inputs
     (list extra-cmake-modules pkg-config kdoctools xorg-server))
    (inputs
     (list breeze-icons
           karchive
           kconfig
           kcrash
           kdbusaddons
           khtml
           ki18n
           kio
           kitemmodels
           kparts
           kpty
           kservice
           kwidgetsaddons
           libarchive
           libzip
           qtbase-5
           zlib
           ;; Command line tools used by Ark.
           lrzip
           lzop
           p7zip
           unzip
           zip
           zstd))
    (home-page "https://apps.kde.org/en/ark")
    (synopsis "Graphical archiving tool")
    (description "Ark is a graphical file compression/decompression utility
with support for multiple formats, including tar, gzip, bzip2, rar and zip, as
well as CD-ROM images.")
    (license license:gpl2+)))

(define-public kate
  (package
    (name "kate")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kate-" version ".tar.xz"))
       (sha256
        (base32 "0nrby307syrqlxrf9lwdzc9c15ifw47418qwszqwg345ma2pww7i"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kactivities
           kconfig
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kitemmodels
           threadweaver
           knewstuff
           kio
           kjobwidgets
           kparts
           ktexteditor
           kwallet
           plasma-framework
           kwindowsystem
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5
           qtscript))
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
    (version "20.04.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kmag-" version ".tar.xz"))
      (sha256
       (base32 "18lk8i2r90gvw8q5j179xgpniih92mwk06krk7w4jv98yinqf6m5"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list ki18n
           kio
           kxmlgui
           oxygen-icons ;; default icon set
           ;; TODO: QAccessibilityClient - libqaccessibilityclien
           qtbase-5))
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
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmousetool-" version ".tar.xz"))
       (sha256
        (base32 "01j6bx8zihns4ip8maj0gb3w3bhx1ha2ljhfmsm6lcyay531ay98"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kcoreaddons
           kconfigwidgets
           kdbusaddons
           ki18n
           kiconthemes
           knotifications
           kxmlgui
           kwindowsystem
           libxtst
           libxt
           phonon
           oxygen-icons ;; default icon set
           qtbase-5))
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
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmouth-" version ".tar.xz"))
       (sha256
        (base32 "1afgxlys9mvmc3rd33g7gchfb0ylx83x3x0a0qf3dra6cpgsgcg7"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5
           qtspeech))
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
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kwidgetsaddons
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5))
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
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kcoreaddons
           kguiaddons
           ki18n
           kiconthemes
           kio
           kitemviews
           knotifications
           kparts
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5
           solid
           zlib))
    (home-page "https://www.krusader.org")
    (synopsis "Twin-panel (commander-style) file manager")
    (description "Krusader is a simple, easy, yet powerful,
twin-panel (commander-style) file manager, similar to Midnight Commander or
Total Commander

It provides all the file management features you could possibly want.  Plus:
extensive archive handling, mounted file system support, FTP, advanced search
module, an internal viewer/editor, directory synchronisation, file content
comparisons, powerful batch renaming and much much more.  It supports a wide
variety of archive formats and can handle other KIO slaves such as smb or
fish.

Almost completely customizable, Krusader is very user friendly, fast and looks
great on your desktop.")
    (license license:gpl2+)))

(define-public kxstitch
  (package
    (name "kxstitch")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kxstitch/" version
                           "/kxstitch-" version ".tar.xz"))
       (sha256
        (base32 "1q6blvcqz6hxdfrkdi0fplmz7rmk3im56kpp68r0yrivhx3hn8sc"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list ktexteditor imagemagick qtbase-5 qtx11extras))
    (home-page "https://kde.org/applications/en/graphics/org.kde.kxstitch")
    (synopsis "Create and print cross stitch patterns")
    (description
     "KXStitch allows creating and printing cross stitch patterns, which can
either be created or generated from a image.")
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
     (list extra-cmake-modules kdoctools qttools shared-mime-info))
    (inputs
     (list kbookmarks
           kcmutils
           kcodecs
           kcrash
           kcompletion
           kconfigwidgets
           kdbusaddons
           ki18n
           kiconthemes
           kio
           knewstuff
           kparts
           kservice
           kwidgetsaddons
           kxmlgui
           oxygen-icons ;; default icon set
           qca
           qtbase-5
           qtscript))
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
    (version "0.12.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/rsibreak/0.12/"
                           "rsibreak-" version ".tar.xz"))
       (sha256
        (base32 "0yjv5awngi2hk6xzlwzmj92i6qppnfc0inqdp16rd8gzfpw7xqqw"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kidletime
           knotifications
           knotifyconfig
           ktextwidgets
           kwindowsystem
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5))
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
    (version "3.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/smb4k/files/"
                           version "/smb4k-" version ".tar.xz/download"))
       (sha256
        (base32 "0hz6nfd845bykf78s4g2qs77szl96gy6g8rpg44pqd39a0k0xbh7"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kconfig
           kconfigwidgets
           kcompletion
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           kjobwidgets
           knotifications
           knotifyconfig
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           samba
           oxygen-icons ;; default icon set
           plasma-framework
           qtbase-5
           qtdeclarative
           solid))
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
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/sweeper-" version ".tar.xz"))
       (sha256
        (base32 "1az3c2khnh51bbmqpamj4p26d3a0ff4l5rd3vcrylg94mk7wgh59"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kactivities-stats
           kbookmarks
           kcrash
           kconfig
           kconfigwidgets
           kcoreaddons
           ki18n
           kio
           ktextwidgets
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5))
    (home-page "https://kde.org/applications/utilities/org.kde.sweeper")
    (synopsis "Temporary file and history cleaner")
    (description "
Sweeper helps to clean unwanted traces the user leaves on the system and to
regain disk space removing unused temporary files.
It can quickly remove temporary information, such as web page cookies,
browser history, or the list of recently-opened documents.  It helps provide
additional privacy on a system shared between multiple users.")
    (license license:lgpl2.0+ )))
