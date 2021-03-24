;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
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

(define-module (gnu packages dunst)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public dunst
  (package
    (name "dunst")
    (version "1.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dunst-project/dunst")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lga1kj2vjbj9g9rl93nivngjmk5fkxdxwal8w96x9whwk9jvdga"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output)
                          (string-append "SYSCONFDIR=" %output "/etc")
                          ;; Otherwise it tries to install service file
                          ;; to "dbus" store directory.
                          (string-append "SERVICEDIR_DBUS=" %output
                                         "/share/dbus-1/services")
                          "dunstify")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)                   ; for pod2man
       ("which" ,which)))
    (inputs
     `(("dbus" ,dbus)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)   ; for svg support
       ("glib" ,glib)
       ("cairo" ,cairo)
       ("pango" ,pango)
       ("libnotify" ,libnotify)         ; for dunstify
       ("libx11" ,libx11)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("libxdg-basedir" ,libxdg-basedir)
       ("wayland" ,wayland)))           ; for wayland support
    (home-page "https://dunst-project.org/")
    (synopsis "Customizable and lightweight notification daemon")
    (description
     "Dunst is a highly configurable and minimalistic notification daemon.
It provides @code{org.freedesktop.Notifications} D-Bus service, so it is
started automatically on the first call via D-Bus.")
    (license license:bsd-3)))
