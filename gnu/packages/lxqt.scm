;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages lxqt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

(define-public libqtxdg
  (package
    (name "libqtxdg")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
         (string-append "https://downloads.lxqt.org/libqtxdg/" version "/"
                        name "-" version ".tar.xz"))
       (sha256
        (base32
         "1ncqs0lcll5nx69hxfg33m3jfkryjqrjhr2kdci0b8pyaqdv1jc8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; test fails with message "Exception"
       #:configure-flags '("-DBUILD_TESTS=ON")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("qt" ,qt))) ; according to Qt5Xdg.pc
    (home-page "https://github.com/lxde/libqtxdg")
    (synopsis "Qt implementation of freedesktop.org xdg specifications")
    (description "Libqtxdg implements the freedesktop.org xdg specifications
in Qt.")
    (license lgpl2.1+)))

(define-public liblxqt
  (package
    (name "liblxqt")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri
         (string-append "https://downloads.lxqt.org/lxqt/" version "/"
                        name "-" version ".tar.xz"))
       (sha256
        (base32
         "0ljdzqavvy82qwwwnhg2bgbshl2ns0k2lcswxlx1cfc8rcdr9w5l"))
       (patches (map search-patch '("liblxqt-include.patch")))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("libqtxdg" ,libqtxdg)))
    (home-page "http://lxqt.org/")
    (synopsis "Core utility library for all LXQt components")
    (description "liblxqt provides the basic libraries shared by the
components of the LXQt desktop environment.")
    (license lgpl2.1+)))


(define-public lxqt-common
  (package
    (name "lxqt-common")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri
         (string-append "https://downloads.lxqt.org/lxqt/" version "/"
                        name "-" version ".tar.xz"))
       (sha256
        (base32
         "0kbkwmrdjhfbq60wf2yfbsjmci8xlw13ilxxa7yxq68n1aqjqmvf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'fix-installation-paths
           (lambda _
             ;; The variable LXQT_ETC_XDG_DIR is set in
             ;; liblxqt-0.9.0/share/cmake/lxqt/lxqt-config.cmake
             ;; to the Qt5 installation directory, followed by "/etc/xdg".
             ;; We need to have it point to the current installation
             ;; directory instead.
             (substitute* '("config/CMakeLists.txt"
                            "menu/CMakeLists.txt")
               (("\\$\\{LXQT_ETC_XDG_DIR\\}")
                "${CMAKE_INSTALL_PREFIX}/etc/xdg")
               ;; In the same file, LXQT_SHARE_DIR is set to the installation
               ;; directory of liblxqt, followed by "/share/lxqt".
               (("\\$\\{LXQT_SHARE_DIR\\}")
                "${CMAKE_INSTALL_PREFIX}/share/lxqt"))
             ;; Replace absolute directories.
             (substitute* "autostart/CMakeLists.txt"
               (("/etc/xdg")
                "${CMAKE_INSTALL_PREFIX}/etc/xdg"))
             (substitute* "xsession/CMakeLists.txt"
               (("/usr/share")
                "${CMAKE_INSTALL_PREFIX}/share")))))))
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)))
    (home-page "http://lxqt.org/")
    (synopsis "Common files for LXQt")
    (description "lxqt-common provides the desktop integration files
(themes, icons, configuration files etc.) for the LXQt
desktop environment.")
    (license lgpl2.1+)))

(define-public lxqt-session
  (package
    (name "lxqt-session")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri
         (string-append "https://downloads.lxqt.org/lxqt/" version "/"
                        name "-" version ".tar.xz"))
       (sha256
        (base32
         "01hxand1gqbcaw14lh7z6w5zssgfaffcjncv752c2c7272wzyhy5"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("kwindowsystem" ,kwindowsystem)
       ("liblxqt" ,liblxqt)
       ("libqtxdg" ,libqtxdg)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'fix-installation-paths
           (lambda _
             ;; The variable LXQT_TRANSLATIONS_DIR is set in
             ;; liblxqt-0.9.0/share/cmake/lxqt/lxqt-config.cmake
             ;; to the liblxqt installation directory, followed by
             ;; "/share/lxqt/translations".
             ;; We need to have it point to the current installation
             ;; directory instead.
             (substitute* '("lxqt-session/CMakeLists.txt"
                            "lxqt-config-session/CMakeLists.txt")
               (("\\$\\{LXQT_TRANSLATIONS_DIR\\}")
                "${CMAKE_INSTALL_PREFIX}/share/lxqt/translations")))))))
    (home-page "http://lxqt.org/")
    (synopsis "Session manager for LXQt")
    (description "lxqt-session provides the standard session manager
for the LXQt desktop environment.")
    (license lgpl2.1+)))
