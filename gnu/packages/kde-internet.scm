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

(define-module (gnu packages kde-internet)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web))

(define-public choqok
  (package
    (name "choqok")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/choqok/"
                           (version-major+minor version)
                           "/src/choqok-" version ".tar.xz"))
       (sha256
        (base32 "03ri4y1wzyqlixnhczsls5gmy7jzzm67bb5gz8bav51ngc32fxca"))
       (patches (search-patches "choqok-Fix-building-under-Qt-5.13.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("attica" ,attica)
       ("kcmutils" ,kcmutils)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdewebkit" ,kdewebkit)
       ("kemoticons" ,kemoticons)
       ("kglobalaccel" ,kglobalaccel)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kparts" ,kparts)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ;; TODO: telepathy
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("qca" ,qca)
       ("qoauth" ,qoauth)
       ("qtbase" ,qtbase)
       ("qtwebkit" ,qtwebkit)
       ("sonnet" ,sonnet)))
    (home-page "https://kde.org/applications/internet/org.kde.choqok")
    (synopsis "Micro-Blogging Client")
    (description "Choqok is a fast, efficient and simple to use micro-blogging
client for KDE.  It currently supports the twitter.com and identi.ca
microblogging services.

Other notable features include:
@itemize
@item Support for user + friends time-lines.
@item Support for @Reply time-lines.
@item Support for sending and receiving direct messages.
@item Twitpic.com integration.
@item The ability to use multiple accounts simultaneously.
@item Support for search APIs for all services.
@item KWallet integration.
@item Support for automatic shortening urls with more than 30 characters.
@item Support for configuring status lists appearance.
@end itemize")
    (license license:gpl3+)))
