;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages kde-plasma)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public libkscreen
  (package
    (name "libkscreen")
    (version "5.13.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "04719va15i66qn1xqx318v6risxhp8bfcnhxh9mqm5h9qx5c6c4k"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ;; For testing.
       ("dbus" ,dbus)))
    (inputs
     `(("kwayland" ,kwayland)
       ("libxrandr" ,libxrandr)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (arguments
     '(#:tests? #f         ; FIXME: 55% tests passed, 5 tests failed out of 11
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; For the missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             ;; Run the tests offscreen.
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://community.kde.org/Solid/Projects/ScreenManagement")
    (synopsis "KDE's screen management software")
    (description "KScreen is the new screen management software for KDE Plasma
Workspaces which tries to be as magic and automatic as possible for users with
basic needs and easy to configure for those who want special setups.")
    (license license:gpl2+)))

(define-public libksysguard
  (package
    (name "libksysguard")
    (version "5.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (sha256
        (base32
         "0k8q5bxk9zyv7c3nny1c399v8acqs618nw39q20pj2qdijl9ibvh"))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kconfigwidgets" ,kconfigwidgets)
       ("kiconthemes" ,kiconthemes)
       ("kwindowsystem" ,kwindowsystem)
       ("ki18n" ,ki18n)
       ("kauth" ,kauth)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kwidgetsaddons" ,kwidgetsaddons)
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
             ;; TODO: Verify: This should no longer be necessary, since
             ;; KF5AuthConfig.cmake.in contains this already.
             (substitute* "processcore/CMakeLists.txt"
               (("KAUTH_HELPER_INSTALL_DIR") "KDE_INSTALL_LIBEXECDIR"))))
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")))
         (replace 'check
           (lambda _
             ;; TODO: Fix this failing test-case
             (invoke "ctest" "-E" "processtest"))))))
    (home-page "https://www.kde.org/info/plasma-5.13.4.php")
    (synopsis "Network enabled task and system monitoring")
    (description "KSysGuard can obtain information on system load and
manage running processes.  It obtains this information by interacting
with a ksysguardd daemon, which may also run on a remote system.")
    (license license:gpl3+)))

