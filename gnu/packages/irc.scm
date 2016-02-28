;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages irc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

(define-public quassel
  (package
    (name "quassel")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://quassel-irc.org/pub/quassel-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "0d6lwf6qblj1ia5j9mjy112zrmpbbg9mmxgscbgxiqychldyjgjd"))))
    (build-system cmake-build-system)
    (arguments
      ;; The three binaries are not mutually exlusive, and are all built
      ;; by default.
     `(#:configure-flags '(;;"-DWANT_QTCLIENT=OFF" ; 5.0 MiB
                           ;;"-DWANT_CORE=OFF" ; 2.3 MiB
                           ;;"-DWANT_MONO=OFF" ; 6.3 MiB
                           "-DUSE_QT5=ON" ; default is qt4
                           "-DWITH_KDE=OFF" ; no to integration
                           "-DWITH_OXYGEN=ON" ; on=embed icons
                           "-DWITH_WEBKIT=ON") ; wants qtwebkit, in qt5
       #:tests? #f)) ; no test target
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("oxygen-icons" ,oxygen-icons)
       ("qca" ,qca)
       ("qt", qt)
       ("snorenotify" ,snorenotify)
       ("zlib" ,zlib)))
    (home-page "http://quassel-irc.org/")
    (synopsis "Distributed IRC client")
    (description "Quassel is a distributed IRC client, meaning that one or more
clients can attach to and detach from the central core.  It resembles the
popular combination of screen and a text-based IRC client such as WeeChat or
irssi, but graphical.")
    (license (list license:gpl2 license:gpl3)))) ;; dual licensed
