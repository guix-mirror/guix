;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages genealogy)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control))

(define-public gramps
  (package
    (name "gramps")
    (version "5.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gramps-project/gramps")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "109dwkswz2h2328xkqk2zj736d117s9pp7rz5cc1qg2vxn1lpm93"))))
    (build-system python-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("intltool" ,intltool)))
    (inputs
     `(("cairo" ,cairo)
       ("font-gnu-freefont" ,font-gnu-freefont)
       ("geocode-glib" ,geocode-glib)
       ("gexiv2" ,gexiv2)
       ("ghostscript" ,ghostscript)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("gtkspell3" ,gtkspell3)
       ("graphviz" ,graphviz)
       ("librsvg" ,librsvg)
       ("osm-gps-map" ,osm-gps-map)
       ("pango" ,pango)
       ("python-bsddb3" ,python-bsddb3)
       ("python-pillow" ,python-pillow)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pyicu" ,python-pyicu)
       ("rcs" ,rcs)
       ("sqlite" ,sqlite)
       ("xdg-utils" ,xdg-utils)))
    (arguments
     `(#:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((ice-9 match)
                  (guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home-for-tests
           (lambda _
             (setenv "HOME" (getenv "TMPDIR"))
             #t))
         (add-before 'wrap 'wrap-with-GI_TYPELIB_PATH
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (paths (map (match-lambda
                                 ((output . directory)
                                  (let ((girepodir (string-append
                                                    directory
                                                    "/lib/girepository-1.0")))
                                    (if (file-exists? girepodir)
                                        girepodir
                                        #f))))
                               inputs)))
               (wrap-program (string-append out "/bin/gramps")
                 `("GI_TYPELIB_PATH" ":" prefix ,(filter identity paths))))
             #t))
         (add-after 'wrap 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (home-page "https://gramps-project.org")
    (synopsis "Genealogical research software")
    (description
     "Gramps is a free software project and community striving to produce
a genealogy program that is both intuitive for hobbyists and feature-complete
for professional genealogists.")
    (license license:gpl2+)))
