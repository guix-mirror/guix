;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
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

(define-module (gnu packages geo)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml))

;;; FIXME GNOME Maps only runs within GNOME. On i3, it fails with this error:
;;; (org.gnome.Maps:8568): GLib-GIO-ERROR **: Settings schema
;;; 'org.gnome.desktop.interface' is not installed
(define-public gnome-maps
  (package
    (name "gnome-maps")
    (version "3.18.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1vdnr2wmhqhql2gxd5n1ijwk88qhim14izbkczncg35846hfsr5i"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags ; Ensure that geoclue is referred to by output.
       (list (string-append "LDFLAGS=-L"
                            (assoc-ref %build-inputs "geoclue") "/lib")
             (string-append "CFLAGS=-I"
                            (assoc-ref %build-inputs "geoclue") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'install 'wrap
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                  (goa-path (string-append
                              (assoc-ref inputs "gnome-online-accounts")
                              "/lib")))
              (wrap-program (string-append out "/bin/gnome-maps")
               `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))

               ;; There seems to be no way to embed the path of libgoa-1.0.so.0.
               `("LD_LIBRARY_PATH" ":" prefix (,goa-path)))
              #t))))))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("folks" ,folks)
       ("libchamplain" ,libchamplain)
       ("libgee" ,libgee)
       ("libxml2" ,libxml2)
       ("geoclue" ,geoclue)
       ("geocode-glib" ,geocode-glib)
       ("gfbgraph" ,gfbgraph)
       ("gjs" ,gjs)
       ("glib" ,glib)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("rest" ,rest)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(("gtk+3" ,gtk+)))
    (synopsis "Graphical map viewer and wayfinding program")
    (description "GNOME Maps is a graphical map viewer.  It uses map data from
the OpenStreetMap project.  It can provide directions for walking, bicycling,
and driving.")
    (home-page "https://wiki.gnome.org/Apps/Maps")
    (license gpl2+)))
