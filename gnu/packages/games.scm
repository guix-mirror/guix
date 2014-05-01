;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2014 David Thompson <dthompson2@worcester.edu>
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

(define-module (gnu packages games)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages sdl)
  #:use-module (guix build-system gnu))

(define-public gnubg
  (package
    (name "gnubg")
    (version "1.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://files.gnubg.org/media/sources/gnubg-release-"
                           version ".000-sources." "tar.gz"))
       (sha256
        (base32
         "015mvjk2iw1cg1kxwxfnvp2rxb9cylf6yc39i30fdy414k07zkky"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)
              ("readline" ,readline)
              ("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("gtkglext" ,gtkglext)
              ("sqlite" ,sqlite)
              ("libcanberra" ,libcanberra)))
    (native-inputs `(("python-2" ,python-2)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://gnubg.org")
    (synopsis "Backgammon game")
    (description "The GNU backgammon application can be used for playing, analyzing and
teaching the game.  It has an advanced evaluation engine based on artificial
neural networks suitable for both beginners and advanced players.  In
addition to a command-line interface, it also features an attractive, 3D
representation of the playing board.")
    (license gpl3+)))

(define-public gnubik
  (package
    (name "gnubik")
    (version "2.4.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnubik/gnubik-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0mfpwz341i1qpzi2qgslpc5i7d4fv7i01kv392m11pczqdc7i7m5"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("libx11" ,libx11)
              ("guile" ,guile-2.0)
              ("gtkglext" ,gtkglext)))
    (native-inputs `(("gettext" ,gnu-gettext)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/gnubik/")
    (synopsis "3d Rubik's cube game")
    (description
     "GNUbik is a puzzle game in which you must manipulate a cube to make
each of its faces have a uniform color.  The game is customizable, allowing
you to set the size of the cube (the default is 3x3) or to change the colors.
 You may even apply photos to the faces instead of colors.  The game is
scriptable with Guile.")
    (license gpl3+)))

(define-public abbaye
  (package
    (name "abbaye")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://abbaye-for-linux.googlecode.com/files/abbaye-for-linux-src-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1wgvckgqa2084rbskxif58wbb83xbas8s1i8s7d57xbj08ryq8rk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases (alist-cons-after
                 'set-paths 'set-sdl-paths
                 (lambda* (#:key inputs outputs (search-paths '()) #:allow-other-keys)
                   (define input-directories
                     (match inputs
                       (((_ . dir) ...)
                        dir)))
                   ;; This package does not use pkg-config, so modify CPATH
                   ;; variable to point to include/SDL for SDL header files.
                   (set-path-environment-variable "CPATH"
                                                  '("include/SDL")
                                                  input-directories))
                 (alist-cons-after
                  'patch-source-shebangs 'patch-makefile
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Replace /usr with package output directory.
                    (for-each (lambda (file)
                                (substitute* file
                                  (("/usr") (assoc-ref outputs "out"))))
                              '("makefile" "src/pantallas.c" "src/comun.h")))
                  (alist-cons-before
                   'install 'make-install-dirs
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((prefix (assoc-ref outputs "out")))
                       ;; Create directories that the makefile assumes exist.
                       (mkdir-p (string-append prefix "/bin"))
                       (mkdir-p (string-append prefix "/share/applications"))))
                   ;; No configure script.
                   (alist-delete 'configure %standard-phases))))
       #:tests? #f)) ;; No check target.
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("sdl" ,sdl)
              ("sdl-gfx" ,sdl-gfx)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("sdl-ttf" ,sdl-ttf)))
    (home-page "http://code.google.com/p/abbaye-for-linux/")
    (synopsis "GNU/Linux port of the indie game \"l'Abbaye des Morts\"")
    (description "L'Abbaye des Morts is a 2D platform game set in 13th century
France.  The Cathars, who preach about good Christian beliefs, were being
expelled by the Catholic Church out of the Languedoc region in France.  One of
them, called Jean Raymond, found an old church in which to hide, not knowing
that beneath its ruins lay buried an ancient evil.")
    (license gpl3+)))
