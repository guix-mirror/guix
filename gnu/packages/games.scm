;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014, 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Sylvain Beucler <beuc@beuc.net>
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016 Rodger Fox <thylakoid@openmailbox.org>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016 Nils Gillmann <niasterisk@grrlz.net>
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
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages mp3)
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
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gstreamer)
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
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages fribidi)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
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
              ("glu" ,glu)
              ("gtkglext" ,gtkglext)
              ("sqlite" ,sqlite)
              ("libcanberra" ,libcanberra)))
    (native-inputs `(("python-2" ,python-2)
                     ("pkg-config" ,pkg-config)))
    (home-page "http://gnubg.org")
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
    (version "2.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnubik/gnubik-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0mhpfnxzbns0wfrsjv5vafqr34770rbvkmdzxk0x0aq67hb3zyl5"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("glu" ,glu)
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
       (uri (string-append "http://abbaye-for-linux.googlecode.com/files/"
                           "abbaye-for-linux-src-" version ".tar.gz"))
       (sha256
        (base32
         "1wgvckgqa2084rbskxif58wbb83xbas8s1i8s7d57xbj08ryq8rk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases (modify-phases %standard-phases
                  (add-after 'set-paths 'set-sdl-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "CPATH"
                              (string-append (assoc-ref inputs "sdl-union")
                                             "/include/SDL"))))
                  (add-after 'patch-source-shebangs 'patch-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Replace /usr with package output directory.
                      (for-each (lambda (file)
                                  (substitute* file
                                    (("/usr") (assoc-ref outputs "out"))))
                                '("makefile" "src/pantallas.c" "src/comun.h"))))
                  (add-before 'install 'make-install-dirs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((prefix (assoc-ref outputs "out")))
                        ;; Create directories that the makefile assumes exist.
                        (mkdir-p (string-append prefix "/bin"))
                        (mkdir-p (string-append prefix "/share/applications"))
                        (mkdir-p (string-append prefix "/share/pixmaps")))))
                  ;; No configure script.
                  (delete 'configure))
       #:tests? #f)) ;; No check target.
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("sdl-union" ,(sdl-union))))
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
              ("glu" ,glu)
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
    (home-page "http://www.asty.org/cmatrix")
    (synopsis "Simulate the display from \"The Matrix\"")
    (description "CMatrix simulates the display from \"The Matrix\" and is
based on the screensaver from the movie's website.  It works with terminal
settings up to 132x300 and can scroll lines all at the same rate or
asynchronously and at a user-defined speed.")
    (license license:gpl2+)))

(define-public chess
  (package
    (name "chess")
    (version "6.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/chess/gnuchess-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1a41ag03q66pwy3pjrmbxxjpzi9fcaiiaiywd7m9v25mxqac2xkp"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/chess")
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
    (version "4.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/xboard/xboard-" version
                           ".tar.gz"))
       (sha256
        (base32
         "05rdj0nyirc4g1qi5hhrjy45y52ihp1j3ldq2c5bwrz0gzy4i3y8"))))
    (build-system gnu-build-system)
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
                  (alist-cons-after
                   'unpack 'apply-patch/mesa-10-fix
                   (lambda* (#:key inputs #:allow-other-keys)
                     (zero? (system* "patch" "--force" "-p3" "-i"
                                     (assoc-ref inputs "patch/mesa-10-fix"))))
                   ;; No configure script
                   (alist-delete 'configure %standard-phases))))
       #:tests? #f ; no check target
       #:make-flags '("CC=gcc" "sharedlib")))
    (native-inputs
     `(("patch/mesa-10-fix" ,(search-patch "irrlicht-mesa-10.patch"))
       ("unzip" ,unzip)))
    (inputs
     `(("mesa" ,mesa)
       ("glu" ,glu)))
    (synopsis "3D game engine written in C++")
    (description
     "The Irrlicht Engine is a high performance realtime 3D engine written in
C++.  Features include an OpenGL renderer, extensible materials, scene graph
management, character animation, particle and other special effects, support
for common mesh file formats, and collision detection.")
    (home-page "http://irrlicht.sourceforge.net/")
    (license license:zlib)))

(define-public mars
  ;; The latest release on SourceForge relies on an unreleased version of SFML
  ;; with a different API, so we take the latest version from the official
  ;; repository on Github.
  (let ((commit   "c855d04409")
        (revision "1"))
    (package
      (name "mars")
      (version (string-append "0.7.5." revision "." commit ))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/thelaui/M.A.R.S..git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1r4c5gap1z2zsv4yjd34qriqkxaq4lb4rykapyzkkdf4g36lc3nh"))
                (patches (list (search-patch "mars-sfml-2.3.patch")
                               (search-patch "mars-install.patch")))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f        ; There are no tests
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-install-path
            (lambda _
              (substitute* "src/CMakeLists.txt"
                (("\\$\\{CMAKE_INSTALL_PREFIX\\}/games")
                 "${CMAKE_INSTALL_PREFIX}/bin"))
              #t))
           (add-after 'unpack 'fix-data-path
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "src/System/settings.cpp"
                (("C_dataPath = \"./data/\";")
                 (string-append "C_dataPath = \""
                                (assoc-ref outputs "out")
                                "/share/games/marsshooter/\";")))
              #t)))))
      (inputs
       `(("mesa" ,mesa)
         ("fribidi" ,fribidi)
         ("taglib" ,taglib)
         ("sfml" ,sfml)))
      (home-page "http://marsshooter.org")
      (synopsis "2D space shooter")
      (description
       "M.A.R.S. is a 2D space shooter with pretty visual effects and
attractive physics.  Players can battle each other or computer controlled
enemies in different game modes such as space ball, death match, team death
match, cannon keep, and grave-itation pit.")
      (license license:gpl3+))))

(define minetest-data
  (package
    (name "minetest-data")
    (version "0.4.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/minetest/minetest_game/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04xnyfap75v3p818kpqb07xshqwqzpws5xjvw5m96qnd9x4725ld"))))
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
    (version "0.4.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/minetest/minetest/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xzrvsg0fpbj5p4pz6lls11m3ab4y2bnjlsm4gnw68x9006ffznp"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
         (list "-DRUN_IN_PLACE=0"
               "-DENABLE_FREETYPE=1"
               "-DENABLE_GETTEXT=1"
               (string-append "-DIRRLICHT_INCLUDE_DIR="
                              (assoc-ref %build-inputs "irrlicht")
                              "/include/irrlicht")
               (string-append "-DCURL_INCLUDE_DIR="
                              (assoc-ref %build-inputs "curl")
                              "/include/curl"))
       #:tests? #f)) ; no check target
    (native-search-paths
     (list (search-path-specification
            (variable "MINETEST_SUBGAME_PATH")
            (files '("share/minetest/games")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("irrlicht" ,irrlicht)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
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

(define glkterm
  (package
   (name "glkterm")
   (version "1.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glk/implementations/glkterm-104.tar.gz"))
     (sha256
      (base32
       "0zlj9nlnkdlvgbiliczinirqygiq8ikg5hzh5vgcmnpg9pvnwga7"))))
   (build-system gnu-build-system)
   (propagated-inputs `(("ncurses" ,ncurses))) ; required by Make.glkterm
   (arguments
    '(#:tests? #f ; no check target
      #:phases
      (alist-replace
       'install
       (lambda* (#:key outputs #:allow-other-keys)
         (let* ((out (assoc-ref outputs "out"))
                (inc (string-append out "/include")))
           (mkdir-p inc)
           (for-each
            (lambda (file)
              (copy-file file (string-append inc "/" file)))
            '("glk.h" "glkstart.h" "gi_blorb.h" "gi_dispa.h" "Make.glkterm"))
           (mkdir (string-append out "/lib"))
           (copy-file "libglkterm.a" (string-append out "/lib/libglkterm.a"))))
       (alist-delete 'configure %standard-phases))))
   (home-page "http://www.eblong.com/zarf/glk/")
   (synopsis "Curses Implementation of the Glk API")
   (description
    "Glk defines a portable API for applications with text UIs.  It was
primarily designed for interactive fiction, but it should be suitable for many
interactive text utilities, particularly those based on a command line.
This is an implementation of the Glk library which runs in a terminal window,
using the curses.h library for screen control.")
   (license (license:fsf-free "file://README"))))

(define-public glulxe
  (package
   (name "glulxe")
   (version "0.5.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glulx/interpreters/glulxe/glulxe-052.tar.gz"))
     (sha256
      (base32
       "19iw6kl8ncqcy9pv4gsqfh3xsa1n94zd234rqavvmxccnf3nj19g"))))
   (build-system gnu-build-system)
   (inputs `(("glk" ,glkterm)))
   (arguments
    '(#:tests? #f ; no check target
      #:make-flags
      (let* ((glk (assoc-ref %build-inputs "glk")))
        (list (string-append "GLKINCLUDEDIR=" glk "/include")
              (string-append "GLKLIBDIR=" glk "/lib")
              (string-append "GLKMAKEFILE=" "Make.glkterm")))
      #:phases
      (alist-replace
       'install
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
           (mkdir-p bin)
           (copy-file "glulxe" (string-append bin "/glulxe"))))
       (alist-delete 'configure %standard-phases))))
   (home-page "http://www.eblong.com/zarf/glulx/")
   (synopsis "Interpreter for Glulx VM")
   (description
    "Glulx is a 32-bit portable virtual machine intended for writing and
playing interactive fiction.  It was designed by Andrew Plotkin to relieve
some of the restrictions in the venerable Z-machine format.  This is the
reference interpreter, using Glk API.")
   (license (license:fsf-free "file://README"))))

(define-public fizmo
  (package
    (name "fizmo")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://christoph-ender.de/fizmo/source/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w7cgyjrhgkadjrazijzhq7zh0pl5bfc6wl7mdpgh020y4kp46d7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (let ((libjpeg (assoc-ref %build-inputs "libjpeg"))
             (ncurses (assoc-ref %build-inputs "ncurses")))
         (list (string-append "jpeg_CFLAGS=-I" libjpeg "/include")
               (string-append "jpeg_LIBS=-ljpeg")
               (string-append "ncursesw_CFLAGS=-I" ncurses "/include")
               (string-append "ncursesw_LIBS=-lncursesw")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libsndfile" ,libsndfile)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("sdl" ,sdl)))
    (home-page "https://christoph-ender.de/fizmo/")
    (synopsis "Z-machine interpreter")
    (description
     "Fizmo is a console-based Z-machine interpreter.  It is used to play
interactive fiction, also known as text adventures, which were implemented
either by Infocom or created using the Inform compiler.")
    (license license:bsd-3)))

(define-public retroarch
  (package
    (name "retroarch")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libretro/RetroArch/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wydzvligyby05x8c4lpg6xcnw9qkmvkskyhzc28xq10vm3q57fv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:phases
       (alist-replace
        'configure
        (lambda _
          (substitute* "qb/qb.libs.sh"
            (("/bin/true") (which "true")))
          (zero? (system*
                  "./configure"
                  (string-append "--prefix=" %output)
                  (string-append "--global-config-dir=" %output "/etc"))))
        %standard-phases)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ffmpeg" ,ffmpeg)
       ("freetype" ,freetype)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python)
       ("sdl" ,sdl2)
       ("udev" ,eudev)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (home-page "http://www.libretro.com/")
    (synopsis "Reference frontend for the libretro API")
    (description
     "Libretro is a simple but powerful development interface that allows for
the easy creation of emulators, games and multimedia applications that can plug
straight into any libretro-compatible frontend.  RetroArch is the official
reference frontend for the libretro API, currently used by most as a modular
multi-system game/emulator system.")
    (license license:gpl3+)))

(define-public gnugo
  (package
    (name "gnugo")
    (version "3.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gnugo/gnugo-" version
                                 ".tar.gz"))
             (sha256
              (base32
               "0wkahvqpzq6lzl5r49a4sd4p52frdmphnqsfdv7gdp24bykdfs6s"))))
    (build-system gnu-build-system)
    (inputs `(("readline" ,readline)))
    (synopsis "Play the game of Go")
    (description
     "GNU Go is a program that plays the game of Go, in which players
place stones on a grid to form territory or capture other stones.  While
it can be played directly from the terminal, rendered in ASCII characters,
it is also possible to play GNU Go with 3rd party graphical interfaces or
even in Emacs.  It supports the standard game storage format (SGF, Smart
Game Format) and inter-process communication format (GMP, Go Modem
Protocol).")
    (home-page "http://www.gnu.org/software/gnugo/")
    (license license:gpl3+)))

(define-public extremetuxracer
  (package
    (name "extremetuxracer")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://downloads.sourceforge.net/project/extremetuxracer/releases/"
                    version "/etr-" version ".tar.xz"))
              (sha256
               (base32
                "0fl9pwkywqnsmgr6plfj9zb05xrdnl5xb2hcmbjk7ap9l4cjfca4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freetype" ,freetype)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("libice" ,libice)
       ("libpng" ,libpng)
       ("sdl" ,sdl)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-image" ,sdl-image)
       ("libsm" ,libsm)
       ("libunwind" ,libunwind)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxmu" ,libxmu)
       ("libxt" ,libxt)
       ("tcl" ,tcl)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-makefile
           (lambda _
             (substitute* "Makefile"
               (("CXXFLAGS =") "CXXFLAGS = ${CFLAGS}")))))))
    (synopsis "High speed arctic racing game based on Tux Racer")
    ;; Snarfed straight from Debian
    (description "Extreme Tux Racer, or etracer as it is called for short, is
a simple OpenGL racing game featuring Tux, the Linux mascot.  The goal of the
game is to slide down a snow- and ice-covered mountain as quickly as possible,
avoiding the trees and rocks that will slow you down.

Collect herrings and other goodies while sliding down the hill, but avoid fish
bones.

This game is based on the GPL version of the famous game TuxRacer.")
    (home-page "http://sourceforge.net/projects/extremetuxracer/")
    (license license:gpl2+)))

(define-public gnujump
  (package
    (name "gnujump")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gnujump/gnujump-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05syy9mzbyqcfnm0hrswlmhwlwx54f0l6zhcaq8c1c0f8dgzxhqk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'link-libm
          (lambda _ (setenv "LIBS" "-lm"))))))
    (inputs
     `(("glu" ,glu)
       ("mesa" ,mesa)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)))
    (home-page "http://gnujump.es.gnu.org/")
    (synopsis
     "Game of jumping to the next floor, trying not to fall")
    (description
     "GNUjump is a simple, yet addictive game in which you must jump from
platform to platform to avoid falling, while the platforms drop at faster rates
the higher you go.  The game features multiplayer, unlimited FPS, smooth floor
falling, themeable graphics and sounds, and replays.")
    (license license:gpl3+)))

(define-public wesnoth
  (package
    (name "wesnoth")
    (version "1.12.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/wesnoth/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "19qyylylaljhk45lk2ja0xp7cx9iy4hx07l65zkg20a2v9h50lmz"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; no check target
       #:configure-flags
       ;; XXX: Failed to compile with '-Werror=old-style-cast'.
       ;;   boost/mpl/assert.hpp:313:58: error:
       ;;     use of old-style cast [-Werror=old-style-cast]
       ;;   [...]
       ;;   cc1plus: all warnings being treated as errors
       '("-DENABLE_STRICT_COMPILATION=OFF")))
    (native-inputs
     `(("gettext" ,gnu-gettext)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("dbus" ,dbus)
       ("fribidi" ,fribidi)
       ("libvorbis" ,libvorbis)
       ("pango" ,pango)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-net" ,sdl-net)
       ("sdl-ttf" ,sdl-ttf)))
    (home-page "http://www.wesnoth.org/")
    (synopsis "Turn-based strategy game")
    (description
     "The Battle for Wesnoth is a fantasy, turn based tactical strategy game,
with several single player campaigns, and multiplayer games (both networked and
local).

Battle for control on a range of maps, using variety of units which have
advantages and disadvantages against different types of attacks.  Units gain
experience and advance levels, and are carried over from one scenario to the
next campaign.")
    (license license:gpl2+)))

(define-public dosbox
  (package
    (name "dosbox")
    (version "0.74.svn3947")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "http://svn.code.sf.net/p/dosbox/code-0/dosbox/trunk/")
                    (revision 3947)))
              (file-name (string-append name "-" version "-checkout"))
              ;; Use SVN head, since the last release (2010) is incompatible
              ;; with GCC 4.8+ (see
              ;; <https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=624976>).
              (sha256
               (base32
                "1p918j6090d1nkvgq7ifvmn506zrdmyi32y7p3ms40d5ssqjg8fj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'autogen.sh
                   (lambda _
                     (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("sdl" ,sdl)
       ("libpng" ,libpng)
       ("zlib" ,zlib)
       ("alsa-lib" ,alsa-lib)
       ("glu" ,glu)
       ("mesa" ,mesa)))
    (home-page "http://www.dosbox.com")
    (synopsis "X86 emulator with CGA/EGA/VGA/etc. graphics and sound")
    (description "DOSBox is a DOS-emulator that uses the SDL library.  DOSBox
also emulates CPU:286/386 realmode/protected mode, Directory
FileSystem/XMS/EMS, Tandy/Hercules/CGA/EGA/VGA/VESA graphics, a
SoundBlaster/Gravis Ultra Sound card for excellent sound compatibility with
older games.")
    (license license:gpl2+)))

(define-public gamine
  (package
    (name "gamine")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gamine-game/"
                                  "gamine-" version ".tar.gz"))
              (sha256
               (base32
                "1iny959i1kl2ab6z5xi4s66mrvrwcarxyvjfp2k1sx532s8knk8h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base) ;playbin plugin
       ("gst-plugins-good" ,gst-plugins-good) ;for wav playback
       ("gtk+" ,gtk+)))
    (arguments
     `(#:tests? #f
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "SYSCONFDIR=" out "/etc")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after
          'install 'wrap-gamine
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out             (assoc-ref outputs "out"))
                  (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
              (wrap-program (string-append out "/bin/gamine")
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
            #t)))))
    (home-page "http://gamine-game.sourceforge.net/")
    (synopsis "Mouse and keyboard discovery for children")
    (description
     "Gamine is a game designed for young children who are learning to use the
mouse and keyboard.  The child uses the mouse to draw colored dots and lines
on the screen and keyboard to display letters.")
    ;; Most files under gpl2+ or gpl3+, but eat.wav under gpl3
    (license license:gpl3)))

(define-public raincat
  (package
    (name "raincat")
    (version "1.1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/Raincat/Raincat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1aalh68h6799mv4vyg30zpskl5jkn6x2j1jza7p4lrflyifxzar8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-random" ,ghc-random)
       ("ghc-glut" ,ghc-glut)
       ("ghc-opengl" ,ghc-opengl)
       ("ghc-sdl" ,ghc-sdl)
       ("ghc-sdl-image" ,ghc-sdl-image)
       ("ghc-sdl-mixer" ,ghc-sdl-mixer)))
    (home-page "http://raincat.bysusanlin.com/")
    (synopsis "Puzzle game with a cat in lead role")
    (description "Project Raincat is a game developed by Carnegie Mellon
students through GCS during the Fall 2008 semester.  Raincat features game
play inspired from classics Lemmings and The Incredible Machine.  The project
proved to be an excellent learning experience for the programmers.  Everything
is programmed in Haskell.")
    (license license:bsd-3)))

(define-public manaplus
  (package
    (name "manaplus")
    (version "1.6.3.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://repo.manaplus.org/manaplus/download/"
                    version "/manaplus-" version ".tar.xz"))
              (sha256
               (base32
                "02bnd4nk1qzrfqckqkwb6sbjzsmacv968ih74cdgcykslpsr684d"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glu" ,glu)
       ("curl" ,curl)
       ("libxml2" ,libxml2)
       ("mesa" ,mesa)
       ("physfs" ,physfs)
       ("sdl-union" ,(sdl-union))))
    (home-page "http://manaplus.org")
    (synopsis "Client for 'The Mana World' and similar games")
    (description
     "ManaPlus is a 2D MMORPG client for game servers.  It is the only
fully supported client for @uref{http://www.themanaworld.org, The mana
world}, @uref{http://evolonline.org, Evol Online} and
@uref{http://landoffire.org, Land of fire}.")
    ;; "src/debug/*" and "src/sdl2gfx/*" are under Zlib.
    ;; "data/themes/{golden-delicious,jewelry}/*" are under CC-BY-SA.
    ;; The rest is under GPL2+.
    (license (list license:gpl2+ license:zlib license:cc-by-sa4.0))))

(define-public mupen64plus-core
  (package
    (name "mupen64plus-core")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-core/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dg2hksm5qni2hcha93k7n4fqr92888p946f7phb0ndschzfh9kk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("freetype" ,freetype)
       ("glu" ,glu)
       ("libpng" ,libpng)
       ("mesa" ,mesa)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "all" (string-append "PREFIX=" out)))
       ;; There are no tests.
       #:tests? #f))
    ;; As per the Makefile (in projects/unix/Makefile):
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Nintendo 64 emulator core library")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
core library.")
    (license license:gpl2+)))

(define-public mupen64plus-audio-sdl
  (package
    (name "mupen64plus-audio-sdl")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-audio-sdl/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ss6w92n2rpfnazhg9lbq0nvs3fqx93nliz3k3wjxdlx4dpi7h3a"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
SDL audio plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-input-sdl
  (package
    (name "mupen64plus-input-sdl")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-input-sdl/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11sj5dbalp2nrlmki34vy7wy28vc175pnnkdk65p8599hnyq37ri"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
SDL input plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-rsp-hle
  (package
    (name "mupen64plus-rsp-hle")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-rsp-hle/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15h7mgz6xd2zjzm6l3f96sbs8kwr3xvbwzgikhnka79m6c69hsxv"))))
    (build-system gnu-build-system)
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
high-level emulation (HLE) RSP processor plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-rsp-z64
  (package
    (name "mupen64plus-rsp-z64")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-rsp-z64/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10jz1w2dhx5slhyk4m8mdqlpsd6cshchslr1fckb2ayzb1ls3ghi"))))
    (build-system gnu-build-system)
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Z64 RSP processor plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-arachnoid
  (package
    (name "mupen64plus-video-arachnoid")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-arachnoid/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jjwf144rihznm4lnqbhgigxw664v3v32wy94adaa6imk8z6gslh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Arachnoid video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-glide64
  (package
    (name "mupen64plus-video-glide64")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-glide64/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rm55dbf6xgsq1blbzs6swa2ajv0qkn38acbljj346abnk6s3dla"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix")))
         ;; XXX Should be unnecessary with the next release.
         (add-before
          'build 'use-sdl2
          (lambda _
            (substitute* "Makefile"
              (("SDL_CONFIG = (.*)sdl-config" all prefix)
               (string-append "SDL_CONFIG = " prefix "sdl2-config"))))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Glide64 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-glide64mk2
  (package
    (name "mupen64plus-video-glide64mk2")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-glide64mk2/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ihl4q293d6svba26b4mhapjcdg12p90gibz79b4mx423jlcxxj9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("boost" ,boost)
       ("libpng" ,libpng)
       ("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Glide64MK2 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-rice
  (package
    (name "mupen64plus-video-rice")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-rice/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rd2scjmh285w61aj3mgx71whg5rqrjbry3cdgicczrnyvf8wdvk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("libpng" ,libpng)
       ("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Rice Video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-z64
  (package
    (name "mupen64plus-video-z64")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-z64/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x7wsjs5gx2iwx20p4cjcbf696zsjlh31qxmghwv0ifrq8x58s1b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("glew" ,glew)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix")))
         ;; XXX Should be unnecessary with the next release.
         (add-before
          'build 'use-sdl2
          (lambda _
            (substitute* "Makefile"
              (("SDL_CONFIG = (.*)sdl-config" all prefix)
               (string-append "SDL_CONFIG = " prefix "sdl2-config"))))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Z64 video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Z64 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-ui-console
  (package
    (name "mupen64plus-ui-console")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-ui-console/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04qkpd8ic7xsgnqz7spl00wxdygf79m7d1k8rabbygjk5lg6p8z2"))
       (patches
        (list (search-patch "mupen64plus-ui-console-notice.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("sdl2" ,sdl2)))
    ;; Mupen64Plus supports a single data directory and a single plugin
    ;; directory in its configuration, yet we need data and plugin files from
    ;; a variety of packages.  The best way to deal with this is to install
    ;; all packages from which data and plugin files are needed into one's
    ;; profile, and point the configuration there.  Hence, propagate the most
    ;; important packages here to save the user from the bother.  The patch
    ;; mupen64plus-ui-console-notice also gives users instructions on what
    ;; they need to do in order to point the configuration to their profile.
    (propagated-inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("mupen64plus-audio-sdl" ,mupen64plus-audio-sdl)
       ("mupen64plus-input-sdl" ,mupen64plus-input-sdl)
       ("mupen64plus-rsp-hle" ,mupen64plus-rsp-hle)
       ("mupen64plus-video-glide64" ,mupen64plus-video-glide64)
       ("mupen64plus-video-glide64mk2" ,mupen64plus-video-glide64mk2)
       ("mupen64plus-video-rice" ,mupen64plus-video-rice)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")
               ;; Trailing slash matters here.
               (string-append "COREDIR=" m64p "/lib/")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
command line user interface.  Installing this package is the easiest way
towards a working Mupen64Plus for casual users.")
    (license license:gpl2+)))

(define-public nestopia-ue
  (package
    (name "nestopia-ue")
    (version "1.46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rdanbrook/nestopia/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07h49xwvg61dx20rk5p4r3ax2ar5y0ppvm60cqwqljyi9rdfbh7p"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; We don't need libretro for the GNU/Linux build.
                  (delete-file-recursively "libretro")
                  ;; Use system zlib.
                  (delete-file-recursively "source/zlib")
                  (substitute* "source/core/NstZlib.cpp"
                    (("#include \"../zlib/zlib.h\"") "#include <zlib.h>"))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("ao" ,ao)
       ("glu" ,glu)
       ("gtk+" ,gtk+)
       ("libarchive" ,libarchive)
       ("mesa" ,mesa)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The Nestopia build system consists solely of a Makefile.
         (delete 'configure)
         ;; XXX Should be unnecessary with the next release.
         (add-before
          'build 'patch-makefile
          (lambda _
            (substitute* "Makefile"
              (("@mkdir \\$@") "@mkdir -p $@")
              (("CC =") "CC ?=")
              (("CXX =") "CXX ?=")
              (("PREFIX =") "PREFIX ?=")
              (("^install:\n$") "install:\n\tmkdir -p $(BINDIR)\n"))))
         (add-before
          'build 'remove-xdg-desktop-menu-call
          (lambda _
            (substitute* "Makefile"
              (("xdg-desktop-menu install .*") "")))))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "CC=gcc" "CXX=g++" (string-append "PREFIX=" out)))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://0ldsk00l.ca/nestopia/")
    (synopsis "Nintendo Entertainment System (NES/Famicom) emulator")
    (description
     "Nestopia UE (Undead Edition) is a fork of the Nintendo Entertainment
System (NES/Famicom) emulator Nestopia, with enhancements from members of the
emulation community.  It provides highly accurate emulation.")
    (license license:gpl2+)))

(define-public emulation-station
  (package
    (name "emulation-station")
    (version "2.0.1")
    (source (origin
              (method git-fetch) ; no tarball available
              (uri (git-reference
                    (url "https://github.com/Aloshi/EmulationStation.git")
                    (commit "646bede"))) ; no version tag
              (sha256
               (base32
                "0cm0sq2wri2l9cvab1l0g02za59q7klj0h3p028vr96n6njj4w9v"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("curl" ,curl)
       ("eigin" ,eigen)
       ("freeimage" ,freeimage)
       ("freetype" ,freetype)
       ("mesa" ,mesa)
       ("sdl2" ,sdl2)))
    (synopsis "Video game console emulator front-end")
    (description "EmulationStation provides a graphical front-end to a large
number of video game console emulators.  It features an interface that is
usable with any game controller that has at least 4 buttons, theming support,
and a game metadata scraper.")
    (home-page "http://www.emulationstation.org")
    (license license:expat)))

(define-public pinball
  (package
    (name "pinball")
    (version "0.3.1")
    (source
     (origin (method url-fetch)
             (uri (string-append "mirror://sourceforge/pinball/pinball/"
                                 "pinball-" version "/"
                                 "pinball-" version ".tar.gz"))
             (sha256
              (base32
               "1f2whlrfidwfh8lvr8cspcyirc6840r5d1ajm7x99qmngygrhixs"))
             (patches (map search-patch
                           '("pinball-const-fix.patch"
                             "pinball-cstddef.patch"
                             "pinball-missing-separators.patch"
                             "pinball-src-deps.patch"
                             "pinball-system-ltdl.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("glu" ,glu)
       ("mesa" ,mesa)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)))
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-image")
                            "/include/SDL -I"
                            (assoc-ref %build-inputs "sdl-mixer")
                            "/include/SDL"))))
    (home-page "http://pinball.sourceforge.net")
    (synopsis "Pinball simulator")
    (description "The Emilia Pinball Project is a pinball simulator.  There
are only two levels to play with, but they are very addictive.")
    (license license:gpl2)))

(define-public pioneers
  (package
    (name "pioneers")
    (version "15.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/pio/"
                                  "pioneers-" version ".tar.gz"))
              (sha256
               (base32
                "128s718nnraiznbg2rajjqb7cfkdg24hy6spdd9narb4f4dsbbv9"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+)
              ("librsvg" ,librsvg)
              ("avahi" ,avahi)))
    (native-inputs `(("intltool" ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "Board game inspired by The Settlers of Catan")
    (description "Pioneers is an emulation of the board game The Settlers of
Catan.  It can be played on a local network, on the internet, and with AI
players.")
    (home-page "http://pio.sourceforge.net/")
    (license license:gpl2+)))

(define-public desmume
  (package
    (name "desmume")
    (version "0.9.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/desmume/desmume/"
             version "/desmume-" version ".tar.gz"))
       (sha256
        (base32
         "15l8wdw3q61fniy3h93d84dnm6s4pyadvh95a0j6d580rjk4pcrs"))))
    (build-system gnu-build-system)
    (arguments
     ;; Enable support for WiFi and microphone.
     `(#:configure-flags '("--enable-wifi"
                           "--enable-openal")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("zlib" ,zlib)
       ("sdl" ,sdl)
       ("glib" ,glib)
       ("gtk+" ,gtk+-2)
       ("glu" ,glu)))
    (home-page "http://desmume.org/")
    (synopsis "Nintendo DS emulator")
    (description
     "DeSmuME is an emulator for the Nintendo DS handheld gaming console.")
    (license license:gpl2)))

(define-public einstein
  (package
    (name "einstein")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://http.debian.net/debian/pool/main/e/"
                                  "einstein/einstein_2.0.dfsg.2.orig.tar.gz"))
              (sha256
               (base32
                "1hxrlv6n8py48j487i6wbb4n4vd55w0na69r7ccmmr9vmrsw5mlk"))
              (patches (list (search-patch "einstein-build.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("freetype" ,freetype)
       ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-ttf)))
       ("zlib" ,zlib)))
    (native-inputs
     `(("font-dejavu" ,font-dejavu)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
        (modify-phases %standard-phases
          (replace 'configure
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (dejavu (string-append (assoc-ref inputs "font-dejavu")
                                         "/share/fonts/truetype/DejaVuSans.ttf")))
              (substitute* "Makefile"
                (("PREFIX=/usr/local") (string-append "PREFIX=" out)))
              ;; The patch above registers a free font for use by the binary,
              ;; but the font is copied during the compile phase into a
              ;; resources file, so we need to make the ttf file available.
              (symlink dejavu "res/DejaVuSans.ttf")
              #t))))))
    (synopsis "Logic puzzle game")
    (description "The goal of this logic game is to open all cards in a 6x6
grid, using a number of hints as to their relative position.  The game idea
is attributed to Albert Einstein.")
    ;; The original home page has disappeared.
    (home-page (string-append "http://web.archive.org/web/20120521062745/"
                              "http://games.flowix.com/en/index.html"))
    ;; License according to
    ;; http://web.archive.org/web/20150222180355/http://www.babichev.info/en/projects/index.html
    ;; The source code is a DFSG-sanitized tarball and does not contain any
    ;; license information.
    (license license:gpl3+)))

(define-public powwow
  (package
    (name "powwow")
    (version "1.2.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.hoopajoo.net/static/projects/powwow-"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xmsg2y7qcvj67i9ilnih0mvfxcpni7fzrz343x9rdfnkkzf3pp8"))))
    (inputs
     `(("ncurses" ,ncurses)))
    (build-system gnu-build-system)
    (home-page "http://www.hoopajoo.net/projects/powwow.html")
    (synopsis "MUD and telnet client")
    (description
     "POWWOW is a client software which can be used for telnet as well as for
@dfn{Multi-User Dungeon} (MUD).  Additionally it can serve as a nice client for
the chat server psyced with the specific config located at
http://lavachat.symlynx.com/unix/")
    (license license:gpl2+)))
