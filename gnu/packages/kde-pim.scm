;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages kde-pim)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml))

(define-public akonadi
  (package
    (name "akonadi")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/akonadi-" version ".tar.xz"))
       (sha256
        (base32 "0v7f1049wjnqxhwxr1443wc2cfbdqmf15xcwjz3j1m0vgdva9pyg"))
       (patches (search-patches
                 "akonadi-paths.patch"
                 "akonadi-timestamps.patch"
                 "akonadi-Revert-Make-installation-properly-relocatabl.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("boost" ,boost)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdesignerplugin" ,kdesignerplugin)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ;; Do NOT add mysql or postgresql to the inputs. Otherwise the binaries
       ;; and wrapped files will refer to them, even if the user choices none
       ;; of these.  Executables are searched on $PATH then.
       ("qtbase" ,qtbase)
       ("sqlite" ,sqlite)))
    (arguments
     `(#:tests? #f ;; TODO 135/167 tests fail
       #:configure-flags '("-DDATABASE_BACKEND=SQLITE") ; lightweight
       #:modules ((ice-9 textual-ports)
                  ,@%qt-build-system-modules)
       #:phases
       (modify-phases (@ (guix build qt-build-system) %standard-phases)
         (add-before 'configure 'add-definitions
           (lambda _
             (let ((out (assoc-ref %outputs "out"))
                   (mysql (assoc-ref %build-inputs "mysql"))
                   (pgsql (assoc-ref %build-inputs "postgresql")))
               (with-output-to-file "CMakeLists.txt.new"
                 (lambda _
                   (display
                    (string-append
                     "add_compile_definitions(\n"
                     "NIX_OUT=\"" out "\"\n"
                     ;; pin binaries for mysql backend
                     ")\n\n"))
                   (display
                    (call-with-input-file "CMakeLists.txt"
                      get-string-all))))
               (rename-file "CMakeLists.txt.new" "CMakeLists.txt"))
             #t)))))
    (home-page "https://kontact.kde.org/components/akonadi.html")
    (synopsis "Extensible cross-desktop storage service for PIM")
    (description "Akonadi is an extensible cross-desktop Personal Information
Management (PIM) storage service.  It provides a common framework for
applications to store and access mail, calendars, addressbooks, and other PIM
data.

This package contains the Akonadi PIM storage server and associated
programs.")
    (license license:fdl1.2+)))
