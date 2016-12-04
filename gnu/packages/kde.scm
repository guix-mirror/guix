;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages qt))

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

(define-public libkomparediff2
  (package
    (name "libkomparediff2")
    (version "16.08.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/KDE/libkomparediff2/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1lafifrwfxvn0jwhz67kwv7m38lm4syips3fq77rwcvfhmkiijmh"))))
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

(define-public libksysguard
  (package
    (name "libksysguard")
    (version "5.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (sha256
        (base32
         "158n30wbpsgbw3axhhsc58hnwhwdd02j3zc9hhcybmnbkfl5c96l"))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kconfigwidgets" ,kconfigwidgets)
       ("kiconthemes" ,kiconthemes)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kinit" ,kinit)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("kwindowsystem" ,kwindowsystem)
       ("kio" ,kio)
       ("ki18n" ,ki18n)
       ("kservice" ,kservice)
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)
       ("qtwebkit" ,qtwebkit)
       ("qtx11extras" ,qtx11extras)
       ("plasma" ,plasma-framework)
       ("zlib" ,zlib)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       `(,(string-append "-DKDE_INSTALL_DATADIR="
                         (assoc-ref %outputs "out") "/share"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-cmakelists
           (lambda _
             (substitute* "processcore/CMakeLists.txt"
               (("KAUTH_HELPER_INSTALL_DIR") "KDE_INSTALL_LIBEXECDIR"))))
         (replace 'check
           (lambda _         ;other tests require a display and therefore fail
             (zero? (system* "ctest" "-R" "chronotest")))))))
    (home-page "https://www.kde.org/info/plasma-5.8.2.php")
    (synopsis "Network enabled task and system monitoring")
    (description "KSysGuard can obtain information on system load and
manage running processes.  It obtains this information by interacting
with a ksysguardd daemon, which may also run on a remote system.")
    (license license:gpl3+)))

(define-public qca
  (package
    (name "qca")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://download.kde.org/stable/qca/" version
                            "/src/qca-" version ".tar.xz"))
        (sha256
         (base32
          "10z9icq28fww4qbzwra8d9z55ywbv74qk68nhiqfrydm21wkxplm"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)
       ("qtbase" ,qtbase)))
    (home-page "http://delta.affinix.com/qca/")
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
        (uri (string-append "http://download.kde.org/stable/snorenotify/"
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
