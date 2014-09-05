;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2014 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Sylvain Beucler <beuc@beuc.net>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Sou Bunnbu <iyzsong@gmail.com>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
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
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages check)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages lua)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial))

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
    (license license:gpl3+)))

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
    (license license:gpl3+)))

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
    (license license:gpl3+)))

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
    (license license:gpl3+)))

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
    (license license:gpl2+)))

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
    (license license:gpl2+)))

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
    (license license:gpl3+)))

(define freedink-engine
  (package
    (name "freedink-engine")
    (version "108.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "08c51imfjfcydm7h0va09z8qfw5nc837bi2x754ni2z737hb5kw2"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags '("--disable-embedded-resources")))
    (native-inputs `(("gettext" ,gnu-gettext)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("sdl" ,sdl)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("sdl-ttf" ,sdl-ttf)
              ("sdl-gfx" ,sdl-gfx)
              ("fontconfig" ,fontconfig)
              ("check" ,check)))
    (home-page "http://www.gnu.org/software/freedink/")
    (synopsis "Twisted adventures of young pig farmer Dink Smallwood")
    (description
     "GNU FreeDink is a free and portable re-implementation of the engine
for the role-playing game Dink Smallwood.  It supports not only the original
game data files but it also supports user-produced game mods or \"D-Mods\".
To that extent, it also includes a front-end for managing all of your D-Mods.")
    (license license:gpl3+)))

(define freedink-data
  (package
    (name "freedink-data")
    (version "1.08.20140901")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-data-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04f1aa8gfz30qkgv7chjz5n1s8v5hbqs01h2113cq1ylm3isd5sp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-delete 'configure (alist-delete 'check %standard-phases))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (home-page "http://www.gnu.org/software/freedink/")
    (synopsis "Game data for GNU Freedink")
    (description
     "This package contains the game data of GNU Freedink.")
    (license license:gpl3+)))

;; TODO: Add freedink-dfarc when there's a wxWidgets package.

(define-public freedink
  ;; This is a wrapper that tells the engine where to find the data.
  (package (inherit freedink-engine)
    (name "freedink")
    (build-system trivial-build-system)
    (arguments
     '(#:builder (begin
                   (use-modules (guix build utils))

                   (let* ((output     (assoc-ref %outputs "out"))
                          (bin        (string-append output "/bin"))
                          (executable (string-append bin "/freedink")))
                     (mkdir-p bin)
                     (call-with-output-file executable
                       (lambda (port)
                         (format port "#!~a/bin/sh
exec ~a/bin/freedink -refdir ~a/share/dink\n"
                                 (assoc-ref %build-inputs "bash")
                                 (assoc-ref %build-inputs "engine")
                                 (assoc-ref %build-inputs "data"))
                         (chmod port #o777)))))
       #:modules ((guix build utils))))
    (inputs `(("engine" ,freedink-engine)
              ("data" ,freedink-data)
              ("bash" ,bash)))
    (native-inputs '())))

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
    (license license:gpl3+)))


(define-public xboing
  (package
    (name "xboing")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.techrescue.org/xboing/xboing"
                           version ".tar.gz"))
       (sha256
        (base32 "16m2si8wmshxpifk861vhpqviqxgcg8bxj6wfw8hpnm4r2w9q0b7"))))
    (arguments
     `(#:tests? #f
       #:phases
       (alist-replace
        'configure 
        (lambda* (#:key outputs #:allow-other-keys)
          
          (substitute* "Imakefile" 
            (("XPMINCLUDE[\t ]*= -I/usr/X11/include/X11")
             (string-append "XPMINCLUDE = -I" (assoc-ref %build-inputs "libxpm")
                            "/include/X11")))
          
          (substitute* "Imakefile" 
            (("XBOING_DIR = \\.") "XBOING_DIR=$(PROJECTROOT)"))
          
          ;; FIXME: HIGH_SCORE_FILE should be set to somewhere writeable
          
          (zero? (system* "xmkmf" "-a" 
                          (string-append "-DProjectRoot=" 
                                         (assoc-ref outputs "out")))))
        (alist-replace 'install
                       (lambda* (#:key outputs #:allow-other-keys)
                         (and
                          (zero? (system* "make" "install.man"))
                          (zero? (system* "make" "install"))))
                       %standard-phases))))
    (inputs `(("libx11" ,libx11)
              ("libxext" ,libxext)
              ("libxpm" ,libxpm)))
    (native-inputs `(("imake" ,imake)
                     ("inetutils" ,inetutils)
                     ("makedepend" ,makedepend)))
    (build-system gnu-build-system)
    (home-page "http://www.techrescue.org/xboing")
    (synopsis "Ball and paddle game")
    (description "XBoing is a blockout type game where you have a paddle which
you control to bounce a ball around the game zone destroying blocks with a
proton ball.  Each block carries a different point value.  The more blocks you
destroy, the better your score.  The person with the highest score wins.")
    (license (license:x11-style "file://COPYING"
                                "Very similar to the X11 licence."))))

(define-public gtypist
  (package
    (name "gtypist")
    (version "2.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gtypist/gtypist-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0xzrkkmj0b1dw3yr0m9hml2y634cc4h61im6zwcq57s7285z8fn1"))
              (modules '((guix build utils)))
              (snippet
               ;; We do not provide `ncurses.h' within an `ncursesw'
               ;; sub-directory, so patch the source accordingly.  See
               ;; <http://bugs.gnu.org/19018>.
               '(for-each (lambda (file)
                            (substitute* file
                              (("ncursesw/ncurses.h")
                               "ncurses.h")))
                          (find-files "." "configure$|\\.c$")))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("perl" ,perl)))
    (home-page "http://www.gnu.org/software/gtypist/")
    (synopsis "Typing tutor")
    (description
     "GNU Typist is a universal typing tutor.  It can be used to learn and
practice touch-typing.  Several tutorials are included; in addition to
tutorials for the standard QWERTY layout, there are also tutorials for the
alternative layouts Dvorak and Colemak, as well as for the numpad.  Tutorials
are primarily in English, however some in other languages are provided.")
    (license license:gpl3+)))

(define-public tiled
  (package
    (name "tiled")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bjorn/tiled/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0p4p3lv4nw11fkfvvyjirb93r2x4w2rrc2w650gl2k57k92jpiij"))))
    (build-system gnu-build-system)
    (inputs `(("qt" ,qt)
              ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (system* "qmake"
                     (string-append "PREFIX=" out))))
        %standard-phases)))
    (home-page "http://www.mapeditor.org/")
    (synopsis "Tile map editor")
    (description
     "Tiled is a general purpose tile map editor.  It is meant to be used for
editing maps of any tile-based game, be it an RPG, a platformer or a Breakout
clone.")

    ;; As noted in 'COPYING', part of it is under GPLv2+, while the rest is
    ;; under BSD-2.
    (license license:gpl2+)))

(define-public openal
  (package
    (name "openal")
    (version "1.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kcat.strangesoft.net/openal-releases/openal-soft-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0mmhdqiyb3c9dzvxspm8h2v8jibhi8pfjxnf6m0wn744y1ia2a8f"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("pulseaudio" ,pulseaudio)))
    (synopsis "3D audio API")
    (description
     "OpenAL provides capabilities for playing audio in a virtual 3D
environment.  Distance attenuation, doppler shift, and directional sound
emitters are among the features handled by the API.  More advanced effects,
including air absorption, occlusion, and environmental reverb, are available
through the EFX extension.  It also facilitates streaming audio, multi-channel
buffers, and audio capture.")
    (home-page "http://kcat.strangesoft.net/openal.html")
    (license license:lgpl2.0+)))

(define-public irrlicht
  (package
    (name "irrlicht")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/irrlicht/Irrlicht%20SDK/"
                    (version-major+minor version)
                    "/" version "/irrlicht-" version ".zip"))
              (sha256
               (base32
                "0yz9lvsc8aqk8wj4rnpanxrw90gqpwn9w5hxp94r8hnm2q0vjjw1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'unpack 'fix-build-env
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "Makefile"
                       (("INSTALL_DIR = /usr/local/lib")
                        (string-append "INSTALL_DIR = " out "/lib")))
                     ;; The Makefile assumes these directories exist.
                     (mkdir-p (string-append out "/lib"))
                     (mkdir-p (string-append out "/include"))))
                 (alist-replace
                  'unpack
                  (lambda* (#:key source #:allow-other-keys)
                    (and (zero? (system* "unzip" source))
                         ;; The actual source is buried a few directories deep.
                         (chdir "irrlicht-1.8.1/source/Irrlicht/")))
                  ;; No configure script
                  (alist-delete 'configure %standard-phases)))
       #:tests? #f ; no check target
       #:make-flags '("CC=gcc" "sharedlib")))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("mesa" ,mesa)))
    (synopsis "3D game engine written in C++")
    (description
     "The Irrlicht Engine is a high performance realtime 3D engine written in
C++.  Features include an OpenGL renderer, extensible materials, scene graph
management, character animation, particle and other special effects, support
for common mesh file formats, and collision detection.")
    (home-page "http://irrlicht.sourceforge.net/")
    (license license:zlib)))

(define minetest-data
  (package
    (name "minetest-data")
    (version "0.4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/minetest/minetest_game/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "09mr71kl7mf4ihszqz1vnwk814p7fvqknad150iic2340a7qzf0i"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("gzip" ,(@ (gnu packages compression) gzip))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((tar (string-append (assoc-ref %build-inputs "tar")
                                             "/bin/tar"))
                         (install-dir (string-append
                                       %output
                                       "/share/minetest/games/minetest_game"))
                         (path (string-append (assoc-ref %build-inputs
                                                         "gzip")
                                              "/bin")))
                     (setenv "PATH" path)
                     (system* tar "xvf" (assoc-ref %build-inputs "source"))
                     (chdir (string-append "minetest_game-" ,version))
                     (mkdir-p install-dir)
                     (copy-recursively "." install-dir)))))
    (synopsis "Main game data for the Minetest game engine")
    (description
     "Game data for the Minetest infinite-world block sandox game.")
    (home-page "http://minetest.net")
    (license license:lgpl2.1+)))

(define-public minetest
  (package
    (name "minetest")
    (version "0.4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/minetest/minetest/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "1xxv0g83iqszjgwnbdcbsprqg76cb6jnbsh5qhm7lcwx4wy2y2k2"))
              ;; This patch will be included in future upstream releases.
              ;; See: https://github.com/minetest/minetest/commit/fd5eaae2babb322f8a3e2acab55a12e218814c8e
              (patches (list (search-patch "minetest-subgame-env-var.patch")))))
    (build-system cmake-build-system)
    (arguments
     '(#:modules ((guix build utils)
                  (guix build cmake-build-system)
                  (ice-9 match))
       #:phases (alist-cons-before
                 'configure 'set-cpath
                 (lambda* (#:key inputs #:allow-other-keys)
                   (use-modules (ice-9 match))
                   ;; Adjust the CPATH so that cmake can find irrlicht,
                   ;; openal, and curl headers.
                   (set-path-environment-variable "CPATH"
                                                  '("include/AL"
                                                    "include/irrlicht"
                                                    "include/curl"
                                                    "include")
                                                  (map (match-lambda
                                                        ((_ . dir) dir))
                                                       inputs)))
                 %standard-phases)
       #:configure-flags '("-DRUN_IN_PLACE=0"
                           "-DENABLE_FREETYPE=1"
                           "-DENABLE_GETTEXT=1")
       #:tests? #f)) ; no check target
    (native-search-paths
     (list (search-path-specification
            (variable "MINETEST_SUBGAME_PATH")
            (directories '("share/minetest/games")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("irrlicht" ,irrlicht)
       ("libpng" ,libpng)
       ("libjpeg-8" ,libjpeg-8)
       ("libxxf86vm" ,libxxf86vm)
       ("mesa" ,mesa)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("openal" ,openal)
       ("freetype" ,(@ (gnu packages fontutils) freetype))
       ("curl" ,curl)
       ("luajit" ,luajit)
       ("gettext" ,gnu-gettext)
       ("sqlite" ,sqlite)))
    (propagated-inputs
     `(("minetest-data" ,minetest-data)))
    (synopsis "Infinite-world block sandbox game")
    (description
     "Minetest is a sandbox construction game.  Players can create and destroy
various types of blocks in a three-dimensional open world.  This allows
forming structures in every possible creation, on multiplayer servers or as a
single player.  Mods and texture packs allow players to personalize the game
in different ways.")
    (home-page "http://minetest.net")
    (license license:lgpl2.1+)))
