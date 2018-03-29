;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018 Alex Kost <alezost@gmail.com>
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
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public dunst
  (package
    (name "dunst")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dunst-project/dunst/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mkdp1vqc376z8clwm5s9070jq1g92j8hv2rr231jr2468fnwaga"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output)
                          ;; Otherwise it tries to install service file
                          ;; to "dbus" store directory.
                          (string-append "SERVICEDIR_DBUS=" %output
                                         "/share/dbus-1/services"))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)                   ; for pod2man
       ("which" ,which)))
    (inputs
     `(("dbus" ,dbus)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("cairo" ,cairo)
       ("pango" ,pango)
       ("libx11" ,libx11)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("libxdg-basedir" ,libxdg-basedir)))
    (home-page "https://dunst-project.org/")
    (synopsis "Customizable and lightweight notification daemon")
    (description
     "Dunst is a highly configurable and minimalistic notification daemon.
It provides 'org.freedesktop.Notifications' D-Bus service, so it is
started automatically on the first call via D-Bus.")
    (license license:bsd-3)))
