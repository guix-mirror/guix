;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2014 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages texinfo)
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

(define-public pingus
  (package
    (name "pingus")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pingus.googlecode.com/files/pingus-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0q34d2k6anzqvb0mf67x85q92lfx9jr71ry13dlp47jx0x9i573m"))
       (patches (list (search-patch "pingus-sdl-libs-config.patch")))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("scons" ,scons)))
    (inputs `(("sdl" ,sdl)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("mesa" ,mesa)
              ("libpng" ,libpng)
              ("boost" ,boost)))
    (arguments
     '(#:tests? #f                      ;no check target
       #:phases
       (alist-delete
        'configure
        (alist-replace
         'install
         (lambda* (#:key outputs #:allow-other-keys)
           (zero? (system* "make" "install"
                           (string-append "PREFIX="
                                          (assoc-ref outputs "out")))))
         %standard-phases))))
    (home-page "http://pingus.seul.org/welcome.html")
    (synopsis "Lemmings clone")
    (description
     "Pingus is a free Lemmings-like puzzle game in which the player takes
command of a bunch of small animals and has to guide them through levels.
Since the animals walk on their own, the player can only influence them by
giving them commands, like build a bridge, dig a hole, or redirect all animals
in the other direction.  Multiple such commands are necessary to reach the
level's exit.  The game is presented in a 2D side view.")
    ;; Some source files are under bsd-3 and gpl2+ licenses.
    (license gpl3+)))

(define-public talkfilters
  (package
    (name "talkfilters")
    (version "2.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.hyperrealm.com/" name "/"
                           name  "-" version  ".tar.gz")) 
       (sha256
        (base32 "19nc5vq4bnkjvhk8srqddzhcs93jyvpm9r6lzjzwc1mgf08yg0a6"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/talkfilters")
    (synopsis "Convert English text to humorous dialects")
    (description "The GNU Talk Filters are programs that convert English text
into stereotyped or otherwise humorous dialects.  The filters are provided as
a C library, so they can easily be integrated into other programs.")
    (license gpl2+)))
 
(define-public cmatrix
  (package
    (name "cmatrix")
    (version "1.2a")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.asty.org/cmatrix/dist/cmatrix-" version
                           ".tar.gz"))
       (sha256
        (base32
         "0k06fw2n8nzp1pcdynhajp5prba03gfgsbj91bknyjr5xb5fd9hz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (alist-replace 'configure
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; This old `configure' script doesn't support
                        ;; variables passed as arguments.
                        (let ((out (assoc-ref outputs "out")))
                          (setenv "CONFIG_SHELL" (which "bash"))
                          (zero?
                           (system* "./configure"
                                    (string-append "--prefix=" out)))))
                      %standard-phases)))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://wwww.asty.org/cmatrix")
    (synopsis "Simulate the display from \"The Matrix\"")
    (description "CMatrix simulates the display from \"The Matrix\" and is
based on the screensaver from the movie's website.  It works with terminal
settings up to 132x300 and can scroll lines all at the same rate or
asynchronously and at a user-defined speed.")
    (license gpl2+)))

(define-public chess
  (package
    (name "chess")
    (version "6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/chess/gnuchess-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1jckpg1qi1vjr3pqs0dnip3rmn0mgklx63xflrpqiv3cx2qlz8kn"))))
    (build-system gnu-build-system)
    (home-page "http://wwww.gnu.org/software/chess")
    (synopsis "Full chess implementation")
    (description "GNU Chess is a chess engine.  It allows you to compete
against the computer in a game of chess, either through the default terminal
interface or via an external visual interface such as GNU XBoard.")
    (license gpl3+)))

(define-public xboard
  (package
    (name "xboard")
    (version "4.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/xboard/xboard-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1amy9krr0qkvcc7gnp3i9x9ma91fc5cq8hy3gdc7rmfsaczv1l3z"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (alist-cons-before 
        'configure 'pre-conf
        ;; This is GNU.  So use gnuchess as the first choice of engine
        (lambda _
          (substitute* "xboard.conf.in" 
            (("-firstChessProgram fairymax") "-firstChessProgram gnuchess")))
        %standard-phases)))
    (inputs `(("cairo" ,cairo)
              ("librsvg" ,librsvg)
              ("libxt" ,libxt)
              ("libxaw" ,libxaw)))
    (native-inputs `(("texinfo" ,texinfo)
                     ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnu.org/software/xboard")
    (synopsis "Graphical user interface for chess programs")
    (description "GNU XBoard is a graphical board for all varieties of chess,
including international chess, xiangqi (Chinese chess), shogi (Japanese chess)
and Makruk.  Several lesser-known variants are also supported.  It presents a
fully interactive graphical interface and it can load and save games in the
Portable Game Notation.")
    (license gpl3+)))
