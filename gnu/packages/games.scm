;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2016 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Sylvain Beucler <beuc@beuc.net>
;;; Copyright © 2014, 2015, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2017, 2018 Christopher Lemmer Webber <cwebber@dustycloud.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016, 2017 Rodger Fox <thylakoid@openmailbox.org>
;;; Copyright © 2016, 2017, 2018 ng0 <ng0@n0.is>
;;; Copyright © 2016 Albin Söderqvist <albin@fripost.org>
;;; Copyright © 2016, 2017, 2018, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Steve Webber <webber.sl@gmail.com>
;;; Copyright © 2017 Adonay "adfeno" Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@hyperbola.info>
;;; Copyright © 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2019 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 okapi <okapi@firemail.cc>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2018 Madalin Ionel-Patrascu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages less)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26))

;; Data package for adanaxisgpl.
(define adanaxis-mush
  (let ((version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (string-append "http://www.mushware.com/files/adanaxis-mush-"
                          version ".tar.gz"))
      (sha256
       (base32 "0mk9ibis5nkdcalcg1lkgnsdxxbw4g5w2i3icjzy667hqirsng03")))))

(define-public adanaxisgpl
  (package
    (name "adanaxisgpl")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.mushware.com/files/adanaxisgpl-"
                           version ".tar.gz"))
       (sha256
        (base32 "0jkn637jaabvlhd6hpvzb57vvjph94l6fbf7qxbjlw9zpr19dw1f"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Necessary for building with gcc >=4.7.
           (substitute* "src/Mushcore/MushcoreSingleton.h"
             (("SingletonPtrSet\\(new SingletonType\\);")
              "MushcoreSingleton::SingletonPtrSet(new SingletonType);"))
           ;; Avoid an "invalid conversion from const char* to char*" error.
           (substitute* "src/Platform/X11/PlatformMiscUtils.cpp"
             (("char \\*end, \\*result;")
              (string-append "const char *end;"
                             "\n"
                             "char *result;")))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((data (assoc-ref inputs "adanaxis-mush"))
                   (share (string-append (assoc-ref outputs "out")
                                         "/share/" ,name "-" ,version)))
               (mkdir-p share)
               (invoke "tar" "xvf" data "-C" share)))))))
    (native-inputs
     `(("adanaxis-mush" ,adanaxis-mush))) ; game data
    (inputs
     `(("expat" ,expat)
       ("freeglut" ,freeglut)
       ("glu" ,glu)
       ("libjpeg" ,libjpeg)
       ("libogg" ,libogg)
       ("libtiff" ,libtiff)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("pcre" ,pcre)
       ("sdl" ,sdl)
       ("sdl-mixer" ,sdl-mixer)))
    (home-page "https://www.mushware.com")
    (synopsis "Action game in four spatial dimensions")
    (description
     "Adanaxis is a fast-moving first person shooter set in deep space, where
the fundamentals of space itself are changed.  By adding another dimension to
space this game provides an environment with movement in four directions and
six planes of rotation.  Initially the game explains the 4D control system via
a graphical sequence, before moving on to 30 levels of gameplay with numerous
enemy, ally, weapon and mission types.  Features include simulated 4D texturing,
mouse and joystick control, and original music.")
    (license license:gpl2)))

(define-public armagetron-advanced
  (package
    (name "armagetron-advanced")
    (version "0.2.8.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/armagetronad/stable/"
                                  version "/armagetronad-" version ".src.tar.gz"))
              (sha256
               (base32
                "1pgy0r80z702qdv94aw3ywdn4ynnr4cdi86ml558pljfc5ygasj4"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxml2" ,libxml2)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("freeglut" ,freeglut)
       ("libpng" ,libpng)
       ("libjpeg-turbo" ,libjpeg-turbo)))
    (home-page "http://www.armagetronad.org")
    (synopsis "Tron clone in 3D")
    (description "Armagetron Advanced is a multiplayer game in 3d that
attempts to emulate and expand on the lightcycle sequence from the movie Tron.
It's an old school arcade game slung into the 21st century.  Highlights
include a customizable playing arena, HUD, unique graphics, and AI bots.  For
the more advanced player there are new game modes and a wide variety of
physics settings to tweak as well.")
    (license license:gpl2+)))

(define-public armagetronad
  (deprecated-package "armagetronad" armagetron-advanced))

(define-public bastet
  (package
    (name "bastet")
    (version "0.43.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fph/bastet.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09kamxapm9jw9przpsgjfg33n9k94bccv65w95dakj0br33a75wn"))
       (patches
        (search-patches "bastet-change-source-of-unordered_set.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CXXFLAGS=-I"
                            (assoc-ref %build-inputs "boost") "/include"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'check
           ;; The 'Test' target builds the tests, but doesn't actually run them.
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "Test" make-flags)
             (setenv "HOME" ".")
             (invoke "./Test")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (share   (string-append out "/share"))
                    (hicolor (string-append share "/icons/hicolor")))
               (install-file "bastet"
                             (string-append out "/bin"))

               (install-file "bastet.desktop"
                             (string-append share "/applications"))
               (install-file "bastet.svg"
                             (string-append hicolor "/scalable/apps"))

               (install-file "bastet.appdata.xml"
                             (string-append share "/appdata"))

               (install-file "bastet.6"
                             (string-append out "/share/man/man6"))
               #t))))))
    (native-inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)))
    (inputs
     `(("boost" ,boost)
       ("ncurses" ,ncurses)))
    (home-page "http://fph.altervista.org/prog/bastet.html")
    (synopsis "Antagonistic Tetris-style falling brick game for text terminals")
    (description
     "Bastet (short for Bastard Tetris) is a simple ncurses-based falling brick
game.  Unlike normal Tetris, Bastet does not choose the next brick at random.
Instead, it uses a special algorithm to choose the worst brick possible.

Playing bastet can be a painful experience, especially if you usually make
canyons and wait for the long I-shaped block to clear four rows at a time.")
    (license license:gpl3+)))

(define-public cataclysm-dark-days-ahead
  (let ((commit "9c732a5de48928691ab863d3ab275ca7b0e522fc"))
    (package
      (name "cataclysm-dda")
      (version "0.D")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/CleverRaven/Cataclysm-DDA.git")
                      (commit commit)))
                (sha256
                 (base32
                  "00zzhx1mh1qjq668cga5nbrxp2qk6b82j5ak65skhgnlr6ii4ysc"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                            "USE_HOME_DIR=1" "DYNAMIC_LINKING=1" "RELEASE=1"
                            "LOCALIZE=1" "LANGUAGES=all")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'build 'build-tiles
             (lambda* (#:key make-flags outputs #:allow-other-keys)
               ;; Change prefix directory and enable tile graphics and sound.
               (apply invoke "make" "TILES=1" "SOUND=1"
                      (string-append "PREFIX="
                                     (assoc-ref outputs "tiles"))
                      (cdr make-flags))))
           (add-after 'install 'install-tiles
             (lambda* (#:key make-flags outputs #:allow-other-keys)
               (apply invoke "make" "install" "TILES=1" "SOUND=1"
                      (string-append "PREFIX="
                                     (assoc-ref outputs "tiles"))
                      (cdr make-flags)))))
         ;; TODO: Add libtap++ from https://github.com/cbab/libtappp as a native
         ;;       input in order to support tests.
         #:tests? #f))
      (outputs '("out"
                 "tiles")) ; For tile graphics and sound support.
      (native-inputs
       `(("gettext" ,gettext-minimal)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("freetype" ,freetype)
         ("libogg" ,libogg)
         ("libvorbis" ,libvorbis)
         ("ncurses" ,ncurses)
         ("sdl2" ,sdl2)
         ("sdl2-image" ,sdl2-image)
         ("sdl2-ttf" ,sdl2-ttf)
         ("sdl2-mixer" ,sdl2-mixer)))
      (home-page "http://en.cataclysmdda.com/")
      (synopsis "Survival horror roguelike video game")
      (description
       "Cataclysm: Dark Days Ahead (or \"DDA\" for short) is a roguelike set
in a post-apocalyptic world.  Struggle to survive in a harsh, persistent,
procedurally generated world.  Scavenge the remnants of a dead civilization
for food, equipment, or, if you are lucky, a vehicle with a full tank of gas
to get you out of Dodge.  Fight to defeat or escape from a wide variety of
powerful monstrosities, from zombies to giant insects to killer robots and
things far stranger and deadlier, and against the others like yourself, that
want what you have.")
      (license license:cc-by-sa3.0))))

(define-public cataclysm-dda
  (deprecated-package "cataclysm-dda" cataclysm-dark-days-ahead))

(define-public cowsay
  (package
    (name "cowsay")
    (version "3.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tnalpgge/"
                                  "rank-amateur-cowsay/archive/"
                                  "cowsay-" version ".tar.gz"))
              (sha256
               (base32
                "12w7apbf6a9qffk92r32b16w22na2fjcqbl32rn0n7zw5hrp3f6q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'build)                ; nothing to be built
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "sh" "install.sh"
                     (assoc-ref outputs "out"))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke (string-append (assoc-ref outputs "out")
                                    "/bin/cowsay")
                     "We're done!"))))))
    (inputs
     `(("perl" ,perl)))
    (home-page (string-append "https://web.archive.org/web/20071026043648/"
                              "http://www.nog.net:80/~tony/warez/cowsay.shtml"))
    (synopsis "Speaking cow text filter")
    (description "Cowsay is basically a text filter.  Send some text into it,
and you get a cow saying your text.  If you think a talking cow isn't enough,
cows can think too: all you have to do is run @command{cowthink}.  If you're
tired of cows, a variety of other ASCII-art messengers are available.")
    (license license:gpl3+)))

(define-public freedoom
  (package
    (name "freedoom")
    (version "0.11.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freedoom/freedoom.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k4dlgr82qk6i7dchp3nybq6awlfag2ivy3zzl1v6vhcrnbvssgl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:parallel-build? #f
       #:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((dejavu (assoc-ref inputs "font-dejavu"))
                    (freedoom (assoc-ref outputs "out"))
                    (wad-dir (string-append freedoom "/share/games/doom")))
               ;; Replace the font-searching function in a shell
               ;; script with a direct path to the required font.
               ;; This is necessary because ImageMagick can only find the
               ;; most basic fonts while in the build environment.
               (substitute* "graphics/titlepic/create_caption"
                 (("font=\\$\\(find_font.*$")
                  (string-append
                   "font=" dejavu
                   "/share/fonts/truetype/DejaVuSansCondensed-Bold.ttf\n")))
               ;; Make icon creation reproducible.
               (substitute* "dist/Makefile"
                 (("freedm.png")
                  "-define png:exclude-chunks=date freedm.png")
                 (("freedoom1.png")
                  "-define png:exclude-chunks=date freedoom1.png")
                 (("freedoom2.png")
                  "-define png:exclude-chunks=date freedoom2.png"))
               ;; Make sure that the install scripts know where to find
               ;; the appropriate WAD files.
               (substitute* "dist/freedoom"
                 (("IWAD=freedm.wad")
                  (string-append "IWAD=" wad-dir "/freedm.wad"))
                 (("IWAD=freedoom1.wad")
                  (string-append "IWAD=" wad-dir "/freedoom1.wad"))
                 (("IWAD=freedoom2.wad")
                  (string-append "IWAD=" wad-dir "/freedoom2.wad")))
               #t))))))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("deutex" ,deutex)
       ("font-dejavu" ,font-dejavu)
       ("imagemagick" ,imagemagick)
       ("python" ,python-2)))
    (inputs
     `(("prboom-plus" ,prboom-plus)))
    (home-page "https://freedoom.github.io/")
    (synopsis "Free content game based on the Doom engine")
    (native-search-paths
     (list (search-path-specification
            (variable "DOOMWADDIR")
            (files '("share/games/doom")))
           (search-path-specification
            (variable "DOOMWADPATH")
            (files '("share/games/doom")))))
    (description
     "The Freedoom project aims to create a complete free content first person
shooter game.  Freedoom by itself is just the raw material for a game: it must
be paired with a compatible game engine (such as @code{prboom-plus}) to be
played.  Freedoom complements the Doom engine with free levels, artwork, sound
effects and music to make a completely free game.")
    (license license:bsd-3)))

(define-public freedroidrpg
  (package
    (name "freedroidrpg")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://ftp.osuosl.org/pub/freedroid/"
                           "freedroidRPG-" (version-major+minor version) "/"
                           "freedroidRPG-" version ".tar.gz"))
       (sha256
        (base32 "0n4kn38ncmcy3lrxmq8fjry6c1z50z4q1zcqfig0j4jb0dsz2va2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append "CFLAGS="
                       "-I" (assoc-ref %build-inputs "sdl-gfx") "/include/SDL "
                       "-I" (assoc-ref %build-inputs "sdl-image") "/include/SDL "
                       "-I" (assoc-ref %build-inputs "sdl-mixer") "/include/SDL")
        "--enable-opengl")
       ;; FIXME: the test suite fails with the following error output:
       ;;   4586 Segmentation fault      env SDL_VIDEODRIVER=dummy \
       ;;   SDL_AUDIODRIVER=dummy ./src/freedroidRPG -nb text
       #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glu" ,glu)
       ("libjpeg" ,libjpeg)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("mesa" ,mesa)
       ("python" ,python-wrapper)
       ("sdl" ,sdl)
       ("sdl-gfx" ,sdl-gfx)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("zlib" ,zlib)))
    (home-page "http://www.freedroid.org/")
    (synopsis "Isometric role-playing game against killer robots")
    (description
     "Freedroid RPG is an @dfn{RPG} (Role-Playing Game) with isometric graphics.
The game tells the story of a world destroyed by a conflict between robots and
their human masters.  To restore peace to humankind, the player must complete
numerous quests while fighting off rebelling robots---either by taking control
of them, or by simply blasting them to pieces with melee and ranged weapons in
real-time combat.")
    (license (list license:expat        ; lua/
                   license:gpl3         ; src/gen_savestruct.py
                   license:gpl2+))))    ; the rest

(define-public golly
  (package
    (name "golly")
    (version "3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/golly/golly/golly-"
                                  version "/golly-" version
                                  "-src.tar.gz"))
              (sha256
               (base32
                "0cg9mbwmf4q6qxhqlnzrxh9y047banxdb8pd3hgj3smmja2zf0jd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "GOLLYDIR="
                                         (assoc-ref %outputs "out")
                                         "/share/golly"))
       #:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             ;; For some reason, setting the PYTHON_SHLIB make flag doesn't
             ;; properly set the path to the Python shared library. This
             ;; substitution acheives the same end by different means.
             (substitute* "gui-wx/wxprefs.cpp"
               (("pythonlib = wxT\\(STRINGIFY\\(PYTHON_SHLIB\\)\\)")
                (string-append "pythonlib = \""
                               (assoc-ref inputs "python")
                               "/lib/libpython-2.7.so\"")))
             #t))
         (replace 'build
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (with-directory-excursion "gui-wx"
               (apply invoke `("make" ,@make-flags "-f" "makefile-gtk")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/golly"))
                    (pixmaps (string-append out "/share/pixmaps"))
                    (share (string-append out "/share/golly")))
               (for-each (lambda (binary)
                           (install-file binary bin))
                         '("bgolly" "golly"))
               (for-each (lambda (document)
                           (install-file
                            (string-append "docs/" document ".html")
                            doc))
                         '("License" "ReadMe" "ToDo"))
               (install-file "gui-wx/icons/appicon.xpm" pixmaps)
               (for-each (lambda (folder)
                           (copy-recursively
                            folder
                            (string-append share "/" folder)))
                         '("Help" "Patterns" "Rules" "Scripts")))
             #t)))))
    (native-inputs
     `(("lua" ,lua)))
    (inputs
     `(("glu" ,glu)
       ("mesa" ,mesa)
       ("python" ,python-2)
       ("wxwidgets" ,wxwidgets-gtk2)
       ("zlib" ,zlib)))
    (home-page "http://golly.sourceforge.net/")
    (synopsis "Software for exploring cellular automata")
    (description
     "Golly simulates Conway's Game of Life and many other types of cellular
automata.  The following features are available:
@enumerate
@item Support for bounded and unbounded universes, with cells of up to 256
  states.
@item Support for multiple algorithms, including Bill Gosper's Hashlife
  algorithm.
@item Loading patterns from BMP, PNG, GIF and TIFF image files.
@item Reading RLE, macrocell, Life 1.05/1.06, dblife and MCell files.
@item Scriptable via Lua or Python.
@item Extracting patterns, rules and scripts from zip files.
@item Downloading patterns, rules and scripts from online archives.
@item Pasting patterns from the clipboard.
@item Unlimited undo/redo.
@item Configurable keyboard shortcuts.
@item Auto fit option to keep patterns within the view.
@end enumerate")
    (license license:gpl2+)))

(define-public meandmyshadow
  (package
    (name "meandmyshadow")
    (version "0.5a")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/meandmyshadow/"
                                  version "/meandmyshadow-" version
                                  "-src.tar.gz"))
              (sha256
               (base32
                "0i98v6cgmpsxy7mbb0s2y6f6qq6mkwzk2nrv1nz39ncf948aky2h"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; there are no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("libarchive" ,libarchive)
       ("lua" ,lua)
       ("sdl" ,(sdl-union (list sdl2
                                sdl2-image
                                sdl2-mixer
                                sdl2-ttf)))))
    (home-page "https://acmepjz.github.io/meandmyshadow/")
    (synopsis "Puzzle/platform game")
    (description "Me and My Shadow is a puzzle/platform game in which you try
to reach the exit by solving puzzles.  Spikes, moving blocks, fragile blocks
and much more stand between you and the exit.  Record your moves and let your
shadow mimic them to reach blocks you couldn't reach alone.")
    (license license:gpl3+)))

(define-public knights
  (package
    (name "knights")
    (version "025")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.knightsgame.org.uk/files/knights_"
                                  version "_src.tar.gz"))
              (sha256
               (base32
                "18vp2ygvn0s0jz8rm585jqf6hjqkam1ximq81k0r9hpmfj7wb88f"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; No configure script.
         (delete 'configure))
       #:tests? #f)) ;; No check target.
    (inputs
     `(("boost" ,boost)
       ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer)))
       ("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("curl" ,curl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.knightsgame.org.uk/")
    (synopsis "Multiplayer dungeon game involving knights and quests")
    (description "Knights is a multiplayer game involving several knights who
must run around a dungeon and complete various quests.  Each game revolves
around a quest – for example, you might have to find some items and carry them
back to your starting point.  This may sound easy, but as there are only
enough items in the dungeon for one player to win, you may end up having to
kill your opponents to get their stuff!  Other quests involve escaping from
the dungeon, fighting a duel to the death against the enemy knights, or
destroying an ancient book using a special wand.")
    ;; This package includes modified sources of lua (X11), enet (Expat), and
    ;; guichan (BSD-3).  The "Coercri" library is released under the Boost
    ;; license.  The whole package is released under GPLv3+.
    (license license:gpl3+)))

(define-public gnubg
  (package
    (name "gnubg")
    (version "1.06.002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://files.gnubg.org/media/sources/gnubg-release-"
                           version "-sources." "tar.gz"))
       (sha256
        (base32
         "11xwhcli1h12k6rnhhyq4jphzrhfik7i8ah3k32pqw803460n6yf"))))
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
    (description "The GNU backgammon application (also known as \"gnubg\") can
be used for playing, analyzing and teaching the game.  It has an advanced
evaluation engine based on artificial neural networks suitable for both
beginners and advanced players.  In addition to a command-line interface, it
also features an attractive, 3D representation of the playing board.")
    (license license:gpl3+)))

(define-public gnubackgammon
  (deprecated-package "gnubackgammon" gnubg))

(define-public gnubik
  (package
    (name "gnubik")
    (version "2.4.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnubik/gnubik-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1vlf924mq8hg93bsjj0rzvs0crc6psmlxyc6zn0fr7msnmpx6gib"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("libx11" ,libx11)
              ("guile" ,guile-2.0)
              ("gtkglext" ,gtkglext)))
    (native-inputs `(("gettext" ,gettext-minimal)
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

(define-public gnushogi
  (package
    (name "gnushogi")
    (version "1.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnushogi/gnushogi-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0a9bsl2nbnb138lq0h14jfc5xvz7hpb2bcsj4mjn6g1hcsl4ik0y"))))
    (arguments `(#:tests? #f)) ;; No check target.
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/gnushogi/")
    (synopsis "The game of Shogi (Japanese chess)")
    (description  "GNU Shogi is a program that plays the game Shogi (Japanese
Chess).  It is similar to standard chess but this variant is far more complicated.")
    (license license:gpl3+)))

(define-public ltris
  (package
    (name "ltris")
    (version "1.0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://prdownloads.sourceforge.net/lgames/"
                           "ltris-" version ".tar.gz"))
       (sha256
        (base32
         "1895wv1fqklrj4apkz47rnkcfhfav7zjknskw6p0886j35vrwslg"))))
    (build-system gnu-build-system)
    (arguments
     '(;; The code in LTris uses traditional GNU semantics for inline functions
       #:configure-flags '("CFLAGS=-fgnu89-inline")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl-union")
                                    "/include/SDL"))
             #t)))))
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl sdl-mixer)))))
    (home-page "http://lgames.sourceforge.net/LTris/")
    (synopsis "Tetris clone based on the SDL library")
    (description
     "LTris is a tetris clone: differently shaped blocks are falling down the
rectangular playing field and can be moved sideways or rotated by 90 degree
units with the aim of building lines without gaps which then disappear (causing
any block above the deleted line to fall down).  LTris has three game modes: In
Classic you play until the stack of blocks reaches the top of the playing field
and no new blocks can enter.  In Figures the playing field is reset to a new
figure each level and later on tiles and lines suddenly appear.  In Multiplayer
up to three players (either human or CPU) compete with each other sending
removed lines to all opponents.  There is also a Demo mode in which you can
watch your CPU playing while enjoying a cup of tea!")
    (license license:gpl2+)))

(define-public nethack
  (package
    (name "nethack")
    (version "3.6.1")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append "https://www.nethack.org/download/" version "/nethack-"
                        (string-join (string-split version #\.) "") "-src.tgz"))
        (sha256
          (base32 "1dha0ijvxhx7c9hr0452h93x81iiqsll8bc9msdnp7xdqcfbz32b"))))
    (inputs
      `(("ncurses" ,ncurses)
        ("bison" ,bison)
        ("flex" ,flex)
        ("less" ,less)))
    (build-system gnu-build-system)
    (arguments
      '(#:make-flags
        `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'patch-paths
            (lambda _
              (substitute* "sys/unix/nethack.sh"
                (("^ *cd .*$") ""))
              (substitute* "sys/unix/Makefile.utl"
                (("^YACC *=.*$") "YACC = bison -y\n")
                (("^LEX *=.*$") "LEX = flex\n")
                (("^# CC = gcc") "CC = gcc"))
              (substitute* "sys/unix/hints/linux"
                (("/bin/gzip") (string-append
                                 (assoc-ref %build-inputs "gzip")
                                 "/bin/gzip"))
                (("^WINTTYLIB=.*") "WINTTYLIB=-lncurses"))
              (substitute* "include/config.h"
                (("^.*define CHDIR.*$") "")
                (("^/\\* *#*define *REPRODUCIBLE_BUILD *\\*/")
                 ;; Honor SOURCE_DATE_EPOCH.
                 "#define REPRODUCIBLE_BUILD"))

              ;; Note: 'makedefs' rejects and ignores dates that are too old
              ;; or too new, so we must choose something reasonable here.
              (setenv "SOURCE_DATE_EPOCH" "1531865062")

              (substitute* "sys/unix/Makefile.src"
                 (("^# CC = gcc") "CC = gcc"))
              #t))
          (replace 'configure
            (lambda _
              (let ((bash (string-append
                            (assoc-ref %build-inputs "bash")
                            "/bin/bash")))
                (with-directory-excursion "sys/unix"
                  (substitute* "setup.sh" (("/bin/sh") bash))
                  (invoke bash "setup.sh" "hints/linux"))
                #t)))
          (add-after 'install 'fixup-paths
            (lambda _
              (let* ((output (assoc-ref %outputs "out"))
                     (nethack-script (string-append output "/bin/nethack")))
                (mkdir-p (string-append output "/games/lib/nethackuserdir"))
                (for-each
                  (lambda (file)
                    (rename-file
                      (string-append output "/games/lib/nethackdir/" file)
                      (string-append output "/games/lib/nethackuserdir/"
                                     file)))
                  '("xlogfile" "logfile" "perm" "record" "save"))
                (mkdir-p (string-append output "/bin"))
                (call-with-output-file nethack-script
                  (lambda (port)
                    (format port "#!~a/bin/sh
PATH=~a:$PATH
if [ ! -d ~~/.config/nethack ]; then
  mkdir -p ~~/.config/nethack
  cp -r ~a/games/lib/nethackuserdir/* ~~/.config/nethack
  chmod -R +w ~~/.config/nethack
fi

RUNDIR=$(mktemp -d)

cleanup() {
  rm -rf $RUNDIR
}
trap cleanup EXIT

cd $RUNDIR
for i in ~~/.config/nethack/*; do
  ln -s $i $(basename $i)
done
for i in ~a/games/lib/nethackdir/*; do
  ln -s $i $(basename $i)
done
~a/games/nethack"
                      (assoc-ref %build-inputs "bash")
                      (list->search-path-as-string
                        (list
                          (string-append
                            (assoc-ref %build-inputs "coreutils") "/bin")
                          (string-append
                            (assoc-ref %build-inputs "less") "/bin"))
                        ":")
                      output
                      output
                      output)))
                (chmod nethack-script #o555)
                #t)))
          (delete 'check))))
    (home-page "https://nethack.org")
    (synopsis "Classic dungeon crawl game")
    (description "NetHack is a single player dungeon exploration game that runs
on a wide variety of computer systems, with a variety of graphical and text
interfaces all using the same game engine.  Unlike many other Dungeons &
Dragons-inspired games, the emphasis in NetHack is on discovering the detail of
the dungeon and not simply killing everything in sight - in fact, killing
everything in sight is a good way to die quickly.  Each game presents a
different landscape - the random number generator provides an essentially
unlimited number of variations of the dungeon and its denizens to be discovered
by the player in one of a number of characters: you can pick your race, your
role, and your gender.")
    (license
      (license:fsdg-compatible
        "https://nethack.org/common/license.html"))))

(define-public pipewalker
  (package
    (name "pipewalker")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://downloads.sourceforge.net/pipewalker/"
                           "pipewalker-" version ".tar.gz"))
       (sha256
        (base32 "1x46wgk0s55562pd96cxagxkn6wpgglq779f9b64ff1k3xzp3myn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-docdir
           ;; Makefile.in ignores configure's ‘--docdir=...’ option.  Fix that.
           (lambda _
             (substitute* "Makefile"
               (("(pkgdocdatadir = ).*" _ assignment)
                (string-append assignment "$(docdir)\n")))
             #t)))))
    (inputs
     `(("libpng" ,libpng)
       ("mesa" ,mesa)
       ("sdl" ,sdl)))
    (home-page "http://pipewalker.sourceforge.net/")
    (synopsis "Logical tile puzzle")
    (description
     "PipeWalker is a simple puzzle game with many diffent themes: connect all
computers to one network server, bring water from a source to the taps, etc.
The underlying mechanism is always the same: you must turn each tile in the
grid in the right direction to combine all components into a single circuit.
Every puzzle has a complete solution, although there may be more than one.")
    (license license:gpl3+)))

(define-public prboom-plus
  (package
   (name "prboom-plus")
   (version "2.5.1.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/prboom-plus/prboom-plus/"
                                version "/prboom-plus-" version ".tar.gz"))
            (sha256
             (base32 "151v6nign86m1a2vqz27krsccpc9m4d1jax4y43v2fa82wfj9qp0"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (substitute* "src/version.c"
                  (("__DATE__") "")
                  (("__TIME__") ""))
                #t))))
   (build-system gnu-build-system)
   (arguments
    '(#:configure-flags '("--disable-cpu-opt")
      #:make-flags `(,(string-append "gamesdir="
                                     (assoc-ref %outputs "out") "/bin"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'set-paths 'set-sdl'paths
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "CPATH"
                    (string-append (assoc-ref inputs "sdl-union")
                                   "/include/SDL"))
            #t)))))
   (inputs
    `(("fluidsynth" ,fluidsynth)
      ("glu" ,glu)
      ("libmad" ,libmad)
      ("libpng" ,libpng)
      ("libvorbis" ,libvorbis)
      ("pcre" ,pcre)
      ("portmidi" ,portmidi)
      ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-net)))))
   (home-page "http://prboom-plus.sourceforge.net/")
   (synopsis "Version of the classic 3D shoot'em'up game Doom")
   (description
    "PrBoom+ is a Doom source port developed from the original PrBoom project.")
   (license license:gpl2+)))

(define-public retux
  (package
    (name "retux")
    (version "1.3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/retux/"
                                  (version-major+minor version) "/retux-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "01bidh4zisjp3nc436x0g85v60dvwb3ig37i7y01sa71j8fm4fmb"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         ;; no setup.py script
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (bin    (string-append out "/bin"))
                    (data   (string-append out "/share/retux"))
                    (doc    (string-append out "/share/doc/retux")))
               (mkdir-p bin)

               (substitute* "retux.py"
                 ;; Use the correct data directory.
                 (("os\\.path\\.join\\(os\\.path\\.dirname\\(__file__\\), \"data\"\\),")
                  (string-append "\"" data "\","))
                 ;; Use Python 3 so the patch-shebangs phase works properly.
                 ((".*python2.*") "#!/usr/bin/python3"))

               (copy-file "retux.py" (string-append bin "/retux"))

               (copy-recursively "data" data)

               (install-file "COPYING" doc)
               #t))))))
    (inputs
     `(("python-sge-pygame" ,python-sge-pygame)
       ("python-six" ,python-six)
       ("python-xsge" ,python-xsge)))
    (home-page "http://retux.nongnu.org")
    (synopsis "Action platformer game")
    (description
     "ReTux is an action platformer loosely inspired by the Mario games,
utilizing the art assets from the @code{SuperTux} project.")
    ;; GPL version 3 or later is the license for the code and some art.
    ;; The rest of the licenses are for the art exclusively, as listed in
    ;; data/LICENSES.
    (license (list license:cc0
                   license:cc-by3.0
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0
                   license:gpl2+
                   license:gpl3+))))

(define-public roguebox-adventures
  (package
    (name "roguebox-adventures")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://download.tuxfamily.org/rba/RogueBoxAdventures_v"
             (string-join (string-split version #\.) "_") "_Source.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "0kmzdgnik8fsf3bg55546l77p3mfxn2awkzfzzdn20n82rd2babw"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (and (invoke "unzip" source)
                  ;; The actual source is buried a few directories deep.
                  (chdir (string-append "RogueBoxAdventures_v"
                                        (string-join
                                         (string-split ,version #\.) "_")
                                        "_Source")))))
         ;; no setup.py script
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (data (string-append
                           out "/share/games/roguebox-adventures")))
               ;; Use the correct data directory.
               (substitute* '("main.py" "LIB/getch.py" "LIB/getch_gcwz.py")
                 (("basic_path + os\\.sep + 'DATA'")
                  (string-append "'" data "'"))
                 (("^basic_path.*$")
                  (string-append "basic_path ='" data "'\n")))
               (substitute* "LIB/gra_files.py"
                 (("basic_path = b_path\\.replace\\('/LIB',''\\)")
                  (string-append "basic_path ='" data "'\n")))

               ;; The game must save in the user's home directory because
               ;; the store is read-only.
               (substitute* "main.py"
                 (("home_save = False") "home_save = True")
                 (("'icon_small.png'")
                  (string-append "'" data "/icon_small.png'"))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (roguebox-adventures
                     (string-append bin "/roguebox-adventures"))
                    (data (string-append
                           out "/share/games/roguebox-adventures"))
                    (lib (string-append data "/LIB"))
                    (doc (string-append
                          out "/share/doc/roguebox-adventures")))
               (mkdir-p bin)
               (mkdir-p doc)

               (for-each (lambda (file)
                           (copy-recursively file
                                             (string-append data "/" file)))
                         '("AUDIO" "FONT" "GRAPHIC" "LIB" "LICENSE"
                           "icon_big.png" "icon_small.png"))
               (for-each (lambda (file)
                           (chmod file #o555)
                           (install-file file lib))
                         '("main.py" "run.py"))

               (copy-recursively "DOC" doc)

               (call-with-output-file
                   roguebox-adventures
                 (lambda (p)
                   (format p "\
#!~a
export PYTHONPATH=~a/LIB:~a
exec -a \"~a\" ~a \"$@\"\n"
                           (which "bash") data (getenv "PYTHONPATH")
                           (which "python3")
                           (string-append lib "/main.py"))))
               (chmod roguebox-adventures #o555))
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("python-pygame" ,python-pygame)
       ("python-tmx" ,python-tmx)))
    (home-page "https://rogueboxadventures.tuxfamily.org")
    (synopsis "A classical roguelike/sandbox game")
    (description
     "RogueBox Adventures is a graphical roguelike with strong influences
from sandbox games like Minecraft or Terraria.  The main idea of RogueBox
Adventures is to offer the player a kind of roguelike toy-world.  This world
can be explored and changed freely.")
    ;; The GPL3+ is for code, the rest are for art.
    (license (list license:cc0
                   license:cc-by3.0
                   license:gpl3+
                   license:silofl1.1))))

(define-public xshogi
  (package
    (name "xshogi")
    (version "1.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnushogi/xshogi-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1dns0nhymak44by18sv48m4xb2skiwbi2i3nb9hl6w9iwd2i2brf"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxaw" ,libxaw)
       ("libxt" ,libxt)))
    (home-page "https://www.gnu.org/software/gnushogi/")
    (synopsis "User interface for gnushogi")
    (description  "A graphical user interface for the package @code{gnushogi}.")
    ;; Contains a copy of GPLv3 but the licence notices simply
    ;; state "GNU General Public Licence" without specifying a version.
    (license license:gpl1+)))

(define-public l-abbaye-des-morts
  (package
    (name "l-abbaye-des-morts")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nevat/abbayedesmorts-gpl.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pwqf7r9bqb2p3xrw9i7y8pgr1401fy3mnnqpb1qkhmdl3gqi9hb"))
       (modules '((guix build utils)))
       (snippet
        ;; Unbundle fonts.
        '(begin
           (delete-file-recursively "fonts")
           #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("CC=gcc")
       #:phases (modify-phases %standard-phases
                  (add-after 'set-paths 'set-sdl-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "CPATH"
                              (string-append (assoc-ref inputs "sdl-union")
                                             "/include/SDL"))))
                  (add-after 'patch-source-shebangs 'patch-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Replace /usr with package output directory.
                      (substitute* "Makefile"
                        (("/usr") (assoc-ref outputs "out")))))
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
    (inputs `(("sdl-union" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer)))))
    (home-page "https://github.com/nevat/abbayedesmorts-gpl")
    (synopsis "GNU/Linux port of the indie game \"l'Abbaye des Morts\"")
    (description "L'Abbaye des Morts is a 2D platform game set in 13th century
France.  The Cathars, who preach about good Christian beliefs, were being
expelled by the Catholic Church out of the Languedoc region in France.  One of
them, called Jean Raymond, found an old church in which to hide, not knowing
that beneath its ruins lay buried an ancient evil.")
    (license license:gpl3)))

(define-public abbaye
  (deprecated-package "abbaye" l-abbaye-des-morts))

(define-public angband
  (package
    (name "angband")
    (version "4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://rephial.org/downloads/"
                           (version-major+minor version)
                           "/angband-" version ".tar.gz"))
       (sha256
        (base32
         "0vs0314lbdc6rzxn4jnb7zp6n1p1cdb8r53savadn7k9vbwc80ll"))
       (modules '((guix build utils)))
       (snippet
        ;; So, some of the sounds/graphics/tilesets are under different
        ;; licenses... some of them even nonfree!  This is a console-only
        ;; version of this package so we just remove them.
        ;; In the future, if someone tries to make a graphical variant of
        ;; this package, they can deal with that mess themselves. :)
        '(begin
           (for-each (lambda (subdir)
                       (let ((lib-subdir (string-append "lib/" subdir)))
                         (delete-file-recursively lib-subdir)))
                     '("fonts" "icons" "sounds" "tiles"))
           (substitute* "lib/Makefile"
             ;; And don't try to invoke makefiles in the directories we removed
             (("gamedata customize help screens fonts tiles sounds icons user")
              "gamedata customize help screens user"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:configure-flags (list (string-append "--bindir=" %output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (substitute* "acinclude.m4"
               (("ncursesw5-config") "ncursesw6-config"))
             (invoke "sh" "autogen.sh"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://rephial.org/")
    (synopsis "Dungeon exploration roguelike")
    (description "Angband is a Classic dungeon exploration roguelike.  Explore
the depths below Angband, seeking riches, fighting monsters, and preparing to
fight Morgoth, the Lord of Darkness.")
    (license license:gpl2)))

(define-public pingus
  (package
    (name "pingus")
    (version "0.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Pingus/pingus.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wp06kcmknsnxz7bjnsndb8x062z7r23fb3yrnbfnj68qhz18y74"))
       (patches (search-patches "pingus-boost-headers.patch"
                                "pingus-sdl-libs-config.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("scons-python2" ,scons-python2)))
    (inputs `(("sdl" ,sdl)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("libpng" ,libpng)
              ("boost" ,boost)))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure script
    (home-page "https://pingus.seul.org/")
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
    (home-page "https://www.gnu.org/software/talkfilters/")
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
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This old ‘configure’ script doesn't support
             ;; variables passed as arguments.
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
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
    (version "6.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/chess/gnuchess-" version
                           ".tar.gz"))
       (sha256
        (base32
         "00j8s0npgfdi41a0mr5w9qbdxagdk2v41lcr42rwl1jp6miyk6cs"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/chess/")
    (synopsis "Full chess implementation")
    (description "GNU Chess is a chess engine.  It allows you to compete
against the computer in a game of chess, either through the default terminal
interface or via an external visual interface such as GNU XBoard.")
    (license license:gpl3+)))

(define freedink-engine
  (package
    (name "freedink-engine")
    (version "109.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "00hhk1bjdrc1np2qz44sa5n1mb62qzwxbvsnws3vpms6iyn3a2sy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-embedded-resources")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-graphical-tests
           (lambda _
             ;; These tests require a graphical interface.
             (substitute* "src/Makefile.am"
               (("test_gfx_fonts TestIOGfxDisplay") ""))
             #t)))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("cxxtest" ,cxxtest)
                     ("gettext" ,gettext-minimal)
                     ("help2man" ,help2man)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("sdl-union" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer
                                             sdl2-ttf sdl2-gfx)))
              ("fontconfig" ,fontconfig)
              ("glm" ,glm)))
    (properties '((ftp-directory . "/freedink")
                  (upstream-name . "freedink")))
    (home-page "https://www.gnu.org/software/freedink/")
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
    (version "1.08.20190120")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-data-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17gvryadlxk172mblbsil7hina1z5wahwaxnr6g3mdq57dvl8pvi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'check))               ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (properties '((ftp-directory . "/freedink")))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Game data for GNU Freedink")
    (description
     "This package contains the game data of GNU Freedink.")
    (license license:gpl3+)))

(define-public freedink-dfarc
  (package
    (name "freedink-dfarc")
    (version "3.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/dfarc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1yp8n3w426xnlp10xk06vfi2y3k9xrcfyck7s7qs1v0ys7n284d5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)))
    (inputs
     `(("bzip2" ,bzip2)
       ("wxwidgets" ,wxwidgets)))
    (properties '((ftp-directory . "/freedink")
                  (upstream-name . "dfarc")))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Front-end for managing and playing Dink Modules")
    (description "DFArc makes it easy to play and manage the GNU FreeDink game
and its numerous D-Mods.")
    (license license:gpl3+)))

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
                         (chmod port #o777)))
                     #t))
       #:modules ((guix build utils))))
    (inputs `(("engine" ,freedink-engine)
              ("data" ,freedink-data)
              ("bash" ,bash)))
    (native-inputs '())))

(define-public xboard
  (package
    (name "xboard")
    (version "4.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/xboard/xboard-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1mkh36xnnacnz9r00b5f9ld9309k32jv6mcavklbdnca8bl56bib"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+" ,gtk+-2)
       ("librsvg" ,librsvg)))
    (native-inputs
     `(("texinfo" ,texinfo)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/xboard/")
    (synopsis "Graphical user interface for chess programs")
    (description "GNU XBoard is a graphical board for all varieties of chess,
including international chess, xiangqi (Chinese chess), shogi (Japanese chess)
and Makruk.  Several lesser-known variants are also supported.  It presents a
fully interactive graphical interface and it can load and save games in the
Portable Game Notation.")
    (license license:gpl3+)))

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
               '(begin
                  (for-each (lambda (file)
                              (substitute* file
                                (("ncursesw/ncurses.h")
                                 "ncurses.h")))
                            (find-files "." "configure$|\\.c$"))
                  #t))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("perl" ,perl)))
    (home-page "https://www.gnu.org/software/gtypist/")
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
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/irrlicht/Irrlicht%20SDK/"
                    (version-major+minor version)
                    "/" version "/irrlicht-" version ".zip"))
              (sha256
               (base32
                "0cz4z4dwrv5ypl19ll67wl6jjpy5k6ly4vr042w4br88qq5jhazl"))
              (patches (search-patches "irrlicht-use-system-libs.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                     '("bin" ; bundled compiled Windows binaries"
                       "source/Irrlicht/MacOSX"
                       "source/Irrlicht/bzip2"
                       "source/Irrlicht/jpeglib"
                       "source/Irrlicht/libpng"
                       "source/Irrlicht/lzma"
                       "source/Irrlicht/zlib"))
                  (delete-file "source/Irrlicht/glext.h")
                  (delete-file "source/Irrlicht/glxext.h")
                  (delete-file "source/Irrlicht/wglext.h")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-source
           (lambda _
             ;; The actual source is buried a few directories deep.
             (chdir "source/Irrlicht/")
             #t))
         (add-after 'chdir-to-source 'fix-build-env
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("INSTALL_DIR = /usr/local/lib")
                  (string-append "INSTALL_DIR = " out "/lib"))
                 ;; Add '-fpermissive' to the CXXFLAGS
                 (("-Wall") "-Wall -fpermissive")) ; CImageLoaderJPG.cpp
               ;; The Makefile assumes these directories exist.
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/include")))))
         (delete 'configure))           ; no configure script
       #:tests? #f                      ; no check target
       #:make-flags '("CC=gcc" "sharedlib")))
    (inputs
     `(("bzip2" ,bzip2)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxxf86vm" ,libxxf86vm)
       ("mesa" ,mesa)))
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
  (let ((commit   "c855d044094a1d92317e38935d81ba938946132e")
        (revision "1"))
    (package
      (name "mars")
      (version (string-append "0.7.5." revision "." (string-take commit 7) ))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/thelaui/M.A.R.S..git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1r4c5gap1z2zsv4yjd34qriqkxaq4lb4rykapyzkkdf4g36lc3nh"))
                (patches (search-patches "mars-sfml-2.3.patch"
                                         "mars-install.patch"))))
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
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/minetest/minetest_game")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "186i1pna2f3fwa2001y8mw5131h0sndhfdxzfqq2gnr1m83sjm0w"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("source" ,source)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((install-dir (string-append
                                       %output
                                       "/share/minetest/games/minetest_game")))
                     (mkdir-p install-dir)
                     (copy-recursively
                       (assoc-ref %build-inputs "source")
                       install-dir)
                     #t))))
    (synopsis "Main game data for the Minetest game engine")
    (description
     "Game data for the Minetest infinite-world block sandox game.")
    (home-page "http://minetest.net")
    (license license:lgpl2.1+)))

(define-public minetest
  (package
    (name "minetest")
    (version "5.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/minetest/minetest")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11i8fqjpdggqfdlx440k5758zy0nbf9phxan9r63mavc7mph88ay"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                   (delete-file-recursively "lib") #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
         (list "-DRUN_IN_PLACE=0"
               "-DENABLE_FREETYPE=1"
               "-DENABLE_GETTEXT=1"
               "-DENABLE_SYSTEM_JSONCPP=TRUE"
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
     `(("curl" ,curl)
       ("freetype" ,(@ (gnu packages fontutils) freetype))
       ("gettext" ,gettext-minimal)
       ("gmp" ,gmp)
       ("irrlicht" ,irrlicht)
       ("jsoncpp" ,jsoncpp)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("libxxf86vm" ,libxxf86vm)
       ("luajit" ,luajit)
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ("openal" ,openal)
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
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (inc (string-append out "/include"))
                   (lib (string-append out "/lib")))
              (for-each
               (lambda (file)
                 (install-file file inc))
               '("glk.h" "glkstart.h" "gi_blorb.h" "gi_dispa.h" "Make.glkterm"))
              (install-file "libglkterm.a" lib))
            #t))
        (delete 'configure))))          ; no configure script
   (home-page "http://www.eblong.com/zarf/glk/")
   (synopsis "Curses Implementation of the Glk API")
   (description
    "Glk defines a portable API for applications with text UIs.  It was
primarily designed for interactive fiction, but it should be suitable for many
interactive text utilities, particularly those based on a command line.
This is an implementation of the Glk library which runs in a terminal window,
using the @code{curses.h} library for screen control.")
   (license (license:fsf-free "file://README"))))

(define-public glulxe
  (package
   (name "glulxe")
   (version "0.5.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glulx/interpreters/glulxe/glulxe-054.tar.gz"))
     (sha256
      (base32
       "0vipydg6ra90yf9b3ipgppwxyb2xdhcxwvirgjy0v20wlf56zhhz"))))
   (build-system gnu-build-system)
   (inputs `(("glk" ,glkterm)))
   (arguments
    '(#:tests? #f                       ; no check target
      #:make-flags
      (let* ((glk (assoc-ref %build-inputs "glk")))
        (list (string-append "GLKINCLUDEDIR=" glk "/include")
              (string-append "GLKLIBDIR=" glk "/lib")
              (string-append "GLKMAKEFILE=" "Make.glkterm")))
      #:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin")))
              (install-file "glulxe" bin))
            #t))
        (delete 'configure))))          ; no configure script
   (home-page "http://www.eblong.com/zarf/glulx/")
   (synopsis "Interpreter for Glulx VM")
   (description
    "Glulx is a 32-bit portable virtual machine intended for writing and
playing interactive fiction.  It was designed by Andrew Plotkin to relieve
some of the restrictions in the venerable Z-machine format.  This is the
reference interpreter, using the Glk API.")
   (license license:expat)))

(define-public fizmo
  (package
    (name "fizmo")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fizmo.spellbreaker.org/source/"
                                  "fizmo-" version ".tar.gz"))
              (sha256
               (base32
                "1amyc4n41jf08kxmdgkk30bzzx54miaxa97w28f417qwn8lrl98w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (let ((libjpeg (assoc-ref %build-inputs "libjpeg"))
             (ncurses (assoc-ref %build-inputs "ncurses")))
         (list (string-append "--with-jpeg-includedir=" libjpeg "/include")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freetype" ,freetype)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libsndfile" ,libsndfile)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("sdl2" ,sdl2)))
    (home-page "https://fizmo.spellbreaker.org/")
    (synopsis "Z-machine interpreter")
    (description
     "Fizmo is a console-based Z-machine interpreter.  It is used to play
interactive fiction, also known as text adventures, which were implemented
either by Infocom or created using the Inform compiler.")
    (license license:bsd-3)))

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
    (home-page "https://www.gnu.org/software/gnugo/")
    (license license:gpl3+)))

(define-public extremetuxracer
  (package
    (name "extremetuxracer")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/extremetuxracer/releases/"
                    version "/etr-" version ".tar.xz"))
              (sha256
               (base32
                "1ly63316c07i0gyqqmyzsyvygsvygn0fpk3bnbg25fi6li99rlsg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glu" ,glu)
       ("sfml" ,sfml)))
    (synopsis "High-speed arctic racing game based on Tux Racer")
    ;; Snarfed straight from Debian.
    (description "Extreme Tux Racer, or etracer as it is called for short, is
a simple OpenGL racing game featuring Tux, the Linux mascot.  The goal of the
game is to slide down a snow- and ice-covered mountain as quickly as possible,
avoiding the trees and rocks that will slow you down.

Collect herrings and other goodies while sliding down the hill, but avoid fish
bones.

This game is based on the GPL version of the famous game TuxRacer.")
    (home-page "https://sourceforge.net/projects/extremetuxracer/")
    (license license:gpl2+)))

(define-public supertuxkart
  (package
    (name "supertuxkart")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/supertuxkart/SuperTuxKart/"
                           version "/supertuxkart-" version "-src.tar.xz"))
       (sha256
        (base32
         "1c4w47ibj87lgwiqygq8qi7jiz6gklj4dwf5bs5zk15s0rqlw0fq"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled library sources
        '(begin
           ;; FIXME: try to unbundle enet, and angelscript
           (for-each delete-file-recursively
                     '("lib/zlib"
                       "lib/libpng"
                       "lib/jpeglib"
                       "lib/glew"
                       "lib/wiiuse"))
           (substitute* "CMakeLists.txt"
             ;; Supertuxkart uses modified versions of the Irrlicht engine
             ;; and the bullet library.  The developers gave an explanation here:
             ;; http://forum.freegamedev.net/viewtopic.php?f=17&t=3906
             (("add_subdirectory\\(.*/(glew|zlib)\"\\)") ""))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:configure-flags
       (list "-DUSE_WIIUSE=0"
             ;; Do not use the bundled zlib
             "-DNO_IRR_COMPILE_WITH_ZLIB_=TRUE"
             ;; FIXME: needs libopenglrecorder
             "-DBUILD_RECORDER=0"
             ;; Irrlicht returns an integer instead of a boolean
             "-DCMAKE_C_FLAGS=-fpermissive")
       #:phases
       (modify-phases %standard-phases
         ;; see https://github.com/supertuxkart/stk-code/issues/3557
         (add-after 'unpack 'patch-for-mesa-18.3
           (lambda _
             (substitute* "src/graphics/gl_headers.hpp"
               (("#if !defined\\(USE_GLES2\\)")
                "#if !defined(USE_GLES2)\n#   define __gl_glext_h_"))
             #t))
         (add-after 'unpack 'unbundle
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("glew")
                (string-append (assoc-ref inputs "glew")
                               "/lib/libGLEW.a"))
               (("include_directories\\(\"\\$\\{PROJECT_SOURCE_DIR\\}/lib/glew/include\"\\)")
                (string-append "include_directories(\""
                               (assoc-ref inputs "glew")
                               "/include\")")))
             #t)))))
    (inputs
     `(("glew" ,glew)
       ("zlib" ,zlib)
       ("openal" ,openal)
       ("libvorbis" ,libvorbis)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("mesa" ,mesa)
       ("libx11" ,libx11)
       ("libxrandr" ,libxrandr)
       ("curl" ,curl)
       ;; The following input is needed to build the bundled and modified
       ;; version of irrlicht.
       ("libjpeg" ,libjpeg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://supertuxkart.net/")
    (synopsis "3D kart racing game")
    (description "SuperTuxKart is a 3D kart racing game, with a focus on
having fun over realism.  You can play with up to 4 friends on one PC, racing
against each other or just trying to beat the computer; single-player mode is
also available.")
    (license license:gpl3+)))

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

(define-public the-battle-for-wesnoth
  (package
    (name "the-battle-for-wesnoth")
    (version "1.14.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/wesnoth/wesnoth-"
                                  (version-major+minor version)
                                  "/wesnoth-" version "/"
                                  "wesnoth-" version ".tar.bz2"))
              (sha256
               (base32
                "0aw3czw3nq8ffakhw2libhvrhnllj61xc5lxpjqv0ig1419s1lj5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no check target
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("dbus" ,dbus)
       ("fribidi" ,fribidi)
       ("libvorbis" ,libvorbis)
       ("openssl" ,openssl)
       ("pango" ,pango)
       ("sdl-union" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))))
    (home-page "https://www.wesnoth.org/")
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

(define-public wesnoth
  (deprecated-package "wesnoth" the-battle-for-wesnoth))

(define-public the-battle-for-wesnoth-server
  (package
    (inherit the-battle-for-wesnoth)
    (name "the-battle-for-wesnoth-server")
    (inputs
     `(("boost" ,boost)
       ("icu4c" ,icu4c)
       ("openssl" ,openssl)
       ("sdl2" ,sdl2)))
    (arguments
     `(#:configure-flags '("-DENABLE_GAME=OFF")
       ,@(package-arguments wesnoth)))
    (synopsis "Dedicated @emph{Battle for Wesnoth} server")
    (description "This package contains a dedicated server for @emph{The
Battle for Wesnoth}.")))

(define-public wesnoth-server
  (deprecated-package "wesnoth-server" the-battle-for-wesnoth-server))

(define-public gamine
  (package
    (name "gamine")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gamine-game/"
                                  "gamine-" version ".tar.gz"))
              (sha256
               (base32
                "1sc6f4445ciigd6yw0ri92746k4hk6ps0bvj9fm1gbp3c3fslk5n"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base) ; playbin plugin
       ("gst-plugins-good" ,gst-plugins-good) ; for wav playback
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

(define-public manaplus
  (package
    (name "manaplus")
    (version "1.7.6.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://repo.manaplus.org/manaplus/download/"
                    version "/manaplus-" version ".tar.xz"))
              (sha256
               (base32
                "0l7swvpzq20am4w2rsjpp6fsvbjv07il6wbfy45a7h9zsdihmqhl"))))
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

(define openttd-engine
  (package
    (name "openttd-engine")
    (version "1.8.0")
    (source
     (origin (method url-fetch)
             (uri (string-append "http://binaries.openttd.org/releases/"
                                 version "/openttd-" version "-source.tar.xz"))
             (patches
              (list
               (origin (method url-fetch)
                       (uri (string-append
                             "https://github.com/OpenTTD/OpenTTD/commit/"
                             "19076c24c1f3baf2a22d1fa832d5688216cf54a3.patch"))
                       (file-name "openttd-fix-compilation-with-ICU-61.patch")
                       (sha256
                        (base32
                         "02d1xmb75yv4x6rfnvxk3vvq4l3lvvwr2pfsdzn7lzalic51ziqh")))))
             (sha256
              (base32
               "0zq8xdg0k92p3s4j9x76591zaqz7k9ra69q008m209vdfffjvly2"))
             (modules '((guix build utils)))
             (snippet
              ;; The DOS port contains proprietary software.
              '(begin
                 (delete-file-recursively "os/dos")
                 #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f              ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; The build process fails if the configure script is passed the
         ;; option "--enable-fast-install".
         (replace 'configure
           (lambda* (#:key inputs outputs (configure-flags '())
                     #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (lzo (assoc-ref inputs "lzo")))
               (apply invoke "./configure"
                      (string-append "--prefix=" out)
                      ;; Provide the "lzo" path.
                      (string-append "--with-liblzo2="
                                     lzo "/lib/liblzo2.a")
                      ;; Put the binary in 'bin' instead of 'games'.
                      "--binary-dir=bin"
                      configure-flags)))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("allegro" ,allegro)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("icu4c" ,icu4c)
       ("libpng" ,libpng)
       ("lzo" ,lzo)
       ("sdl" ,sdl)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (synopsis "Transportation economics simulator game")
    (description "OpenTTD is a game in which you transport goods and
passengers by land, water and air.  It is a re-implementation of Transport
Tycoon Deluxe with many enhancements including multiplayer mode,
internationalization support, conditional orders and the ability to clone,
autoreplace and autoupdate vehicles.  This package only includes the game
engine.  When you start it you will be prompted to download a graphics set.")
    (home-page "http://openttd.org/")
    ;; This package is GPLv2, except for a few files located in
    ;; "src/3rdparty/" which are under the 3-clause BSD, LGPLv2.1+ and Zlib
    ;; licenses.  In addition, this software contains an in-game downloader
    ;; from which the user may find non-functional data licensed under
    ;; different terms.
    (license (list license:bsd-3 license:gpl2 license:lgpl2.1+ license:zlib))))

(define openttd-opengfx
  (package
    (name "openttd-opengfx")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://binaries.openttd.org/extra/opengfx/"
                           version "/opengfx-" version "-source.tar.xz"))
       (sha256
        (base32
         "0iz66q7p1mf00njfjbc4vibh3jaybki7armkl18iz7p6x4chp9zv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "INSTALL_DIR="
                                         (assoc-ref %outputs "out")
                                         "/share/games/openttd/baseset/opengfx"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; Make sure HOME is writable for GIMP.
             (setenv "HOME" (getcwd))

             ;; Redirect stdout, not stderr, to /dev/null. This prevents
             ;; dos2unix from receiving its version information as a flag.
             (substitute* "Makefile"
               (("\\$\\(UNIX2DOS\\) -q --version 2>/dev/null")
                "$(UNIX2DOS) -q --version 1>/dev/null")))))
       ;; The check phase for this package only checks the md5sums of the built
       ;; GRF files against the md5sums of the release versions. Because we use
       ;; different software versions than upstream does, some of the md5sums
       ;; are different. However, the package is still reproducible, it's safe
       ;; to disable this test.
       #:tests? #f
       #:parallel-build? #f))
    (native-inputs `(("dos2unix" ,dos2unix)
                     ("gimp" ,gimp)
                     ("grfcodec" ,grfcodec)
                     ("nml" ,nml)
                     ("python" ,python-2)))
    (home-page "http://dev.openttdcoop.org/projects/opengfx")
    (synopsis "Base graphics set for OpenTTD")
    (description
     "The OpenGFX projects is an implementation of the OpenTTD base grahics
set that aims to ensure the best possible out-of-the-box experience.

OpenGFX provides you with...
@enumerate
@item All graphics you need to enjoy OpenTTD.
@item Uniquely drawn rail vehicles for every climate.
@item Completely snow-aware rivers.
@item Different river and sea water.
@item Snow-aware buoys.
@end enumerate")
    (license license:gpl2)))

(define openttd-opensfx
  (package
    (name "openttd-opensfx")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://binaries.openttd.org/extra/opensfx/"
             version "/opensfx-" version "-source.tar.gz"))
       (sha256
        (base32
         "03jxgp02ks31hmsdh4xh0xcpkb70ds8jakc9pfc1y9vdrdavh4p5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("catcodec" ,catcodec)
       ("python" ,python-2)))
    (arguments
     `(#:make-flags
       (list (string-append "INSTALL_DIR=" %output
                            "/share/games/openttd/baseset/opensfx"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; Remove the time dependency of the installed tarball by setting
             ;; the modification times if its members to 0.
             (substitute* "scripts/Makefile.def"
               (("-cf") " --mtime=@0 -cf"))
             #t))
         (delete 'configure))))
    (home-page "http://dev.openttdcoop.org/projects/opensfx")
    (synopsis "Base sounds for OpenTTD")
    (description "OpenSFX is a set of free base sounds for OpenTTD which make
it possible to play OpenTTD without requiring the proprietary sound files from
the original Transport Tycoon Deluxe.")
    (license license:cc-sampling-plus-1.0)))

(define openttd-openmsx
  (package
    (name "openttd-openmsx")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://binaries.openttd.org/extra/openmsx/"
             version "/openmsx-" version "-source.tar.gz"))
       (sha256
        (base32
         "0nskq97a6fsv1v6d62zf3yb8whzhqnlh3lap3va3nzvj7csjgf7c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (arguments
     `(#:make-flags
       (list (string-append "INSTALL_DIR=" %output
                            "/share/games/openttd/baseset"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'post-install
           ;; Rename openmsx-version to openmsx
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((install-directory (string-append (assoc-ref outputs "out")
                                                     "/share/games/openttd/baseset")))
               (rename-file (string-append install-directory "/openmsx-" ,version)
                            (string-append install-directory "/openmsx"))
               #t))))))
    (home-page "http://dev.openttdcoop.org/projects/openmsx")
    (synopsis "Music set for OpenTTD")
    (description "OpenMSX is a music set for OpenTTD which makes it possible
to play OpenTTD without requiring the proprietary music from the original
Transport Tycoon Deluxe.")
    (license license:gpl2)))

(define-public openttd
  (package
    (inherit openttd-engine)
    (name "openttd")
    (arguments
     `(#:configure-flags
       (list (string-append "--with-midi=" (assoc-ref %build-inputs "timidity++")
                            "/bin/timidity"))
       ,@(substitute-keyword-arguments (package-arguments openttd-engine)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'install 'install-data
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (for-each
                    (lambda (input)
                      (copy-recursively (assoc-ref inputs input)
                                        (assoc-ref outputs "out")))
                    (list "opengfx" "openmsx" "opensfx"))
                   #t)))))))
    (inputs
     `(("timidity++" ,timidity++)
       ,@(package-inputs openttd-engine)))
    (native-inputs
     `(("opengfx" ,openttd-opengfx)
       ("openmsx" ,openttd-openmsx)
       ("opensfx" ,openttd-opensfx)
       ,@(package-native-inputs openttd-engine)))))

(define openrct2-title-sequences
  (package
   (name "openrct2-title-sequences")
   (version "0.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/OpenRCT2/title-sequences/releases/download/v"
                         version "/title-sequence-v" version ".zip"))
     (file-name (string-append name "-" version ".zip"))
     (sha256
      (base32
       "0qbyxrsw8hlgaq0r5d7lx7an3idy4qbfv7yiw9byhldk763n9cfw"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("unzip" ,unzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (openrct2-title-sequences (string-append out
                                         "/share/openrct2/title-sequences"))
               (source (assoc-ref %build-inputs "source"))
               (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
          (copy-file source (string-append ,name "-" ,version ".zip"))
          (invoke unzip (string-append ,name "-" ,version ".zip"))
          (delete-file (string-append ,name "-" ,version ".zip"))
          (mkdir-p openrct2-title-sequences)
          (copy-recursively "."
                            openrct2-title-sequences)
          #t))))
   (home-page "https://github.com/OpenRCT2/OpenRCT2")
   (synopsis "Title sequences for OpenRCT2")
   (description
    "openrct2-title-sequences is a set of title sequences for OpenRCT2.")
   (license license:gpl3+)))

(define openrct2-objects
  (package
   (name "openrct2-objects")
   (version "1.0.9")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/OpenRCT2/objects/releases/download/v"
                         version "/objects.zip"))
     (file-name (string-append name "-" version ".zip"))
     (sha256
      (base32 "02apb8h553m7d6jvysgb1zahvxc1yzyygfca2iclb21b3fhpsas4"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("unzip" ,unzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (openrct2-objects (string-append out
                                         "/share/openrct2/objects"))
               (source (assoc-ref %build-inputs "source"))
               (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
          (copy-file source (string-append ,name "-" ,version ".zip"))
          (invoke unzip (string-append ,name "-" ,version ".zip"))
          (delete-file (string-append ,name "-" ,version ".zip"))
          (mkdir-p openrct2-objects)
          (copy-recursively "."
                            openrct2-objects)
          #t))))
   (home-page "https://github.com/OpenRCT2/OpenRCT2")
   (synopsis "Objects for OpenRCT2")
   (description
    "openrct2-objects is a set of objects for OpenRCT2.")
   (license license:gpl3+)))

(define-public openrct2
  (package
    (name "openrct2")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenRCT2/OpenRCT2.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bfqmb6cbmsjcvj77vppy5lw1m4lkvxd1w3f218ah4788xnkysq2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DDOWNLOAD_OBJECTS=OFF"
                               "-DDOWNLOAD_TITLE_SEQUENCES=OFF")
       #:tests? #f                      ; tests require network access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-usr-share-paths&add-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((titles (assoc-ref inputs "openrct2-title-sequences"))
                   (objects (assoc-ref inputs "openrct2-objects")))
               ;; Fix some references to /usr/share.
               (substitute* "src/openrct2/platform/Platform.Linux.cpp"
                 (("/usr/share")
                  (string-append (assoc-ref %outputs "out") "/share")))
               (copy-recursively
                (string-append titles "/share/openrct2/title-sequences")
                "data/title")
               (copy-recursively
                (string-append objects "/share/openrct2/objects")
                "data/object"))))
         (add-before 'configure 'fixgcc7
           (lambda _
             (unsetenv "C_INCLUDE_PATH")
             (unsetenv "CPLUS_INCLUDE_PATH")
             #t))
         (add-after 'fixgcc7 'get-rid-of-errors
           (lambda _
             ;; Don't treat warnings as errors.
             (substitute* "CMakeLists.txt"
               (("-Werror") "")))))))
    (inputs `(("curl" ,curl)
              ("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("icu4c" ,icu4c)
              ("jansson" ,jansson)
              ("libpng" ,libpng)
              ("libzip" ,libzip)
              ("mesa" ,mesa)
              ("openrct2-objects" ,openrct2-objects)
              ("openrct2-title-sequences" ,openrct2-title-sequences)
              ("openssl" ,openssl)
              ("sdl2" ,sdl2)
              ("speexdsp" ,speexdsp)
              ("zlib" ,zlib)))
    (native-inputs
     `(("gcc" ,gcc-7)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/OpenRCT2/OpenRCT2")
    (synopsis "Free software re-implementation of RollerCoaster Tycoon 2")
    (description "OpenRCT2 is a free software re-implementation of
RollerCoaster Tycoon 2 (RCT2).  The gameplay revolves around building and
maintaining an amusement park containing attractions, shops and facilities.

Note that this package does @emph{not} provide the game assets (sounds,
images, etc.)")
    ;; See <https://github.com/OpenRCT2/OpenRCT2/wiki/Required-RCT2-files>
    ;; regarding assets.
    (license license:gpl3+)))

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
             (patches (search-patches "pinball-const-fix.patch"
                                      "pinball-cstddef.patch"
                                      "pinball-missing-separators.patch"
                                      "pinball-src-deps.patch"
                                      "pinball-system-ltdl.patch"))))
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
    (version "15.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/pio/"
                                  "pioneers-" version ".tar.gz"))
              (sha256
               (base32
                "037gdiiw690jw3wd1s9lxmkqx0caxyk0b4drpm7i9p28gig43q9y"))))
    (build-system gnu-build-system)
    (inputs `(("avahi" ,avahi)
              ("gtk+" ,gtk+)
              ("librsvg" ,librsvg)))
    (native-inputs `(("intltool" ,intltool)
                     ("itstool" ,itstool)
                     ("libxml2" ,libxml2)
                     ("pkg-config" ,pkg-config)))
    (synopsis "Board game inspired by The Settlers of Catan")
    (description "Pioneers is an emulation of the board game The Settlers of
Catan.  It can be played on a local network, on the internet, and with AI
players.")
    (home-page "http://pio.sourceforge.net/")
    (license license:gpl2+)))

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
              (patches (search-patches "einstein-build.patch"))))
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
    (version "1.2.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.hoopajoo.net/static/projects/powwow-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1gf0jc1vfv05lxij51n3c1dqn3aiiy2kj1v6q14an3wm7yl7cllp"))))
    (inputs
     `(("ncurses" ,ncurses)))
    (build-system gnu-build-system)
    (home-page "https://www.hoopajoo.net/projects/powwow.html")
    (synopsis "MUD and telnet client")
    (description
     "POWWOW is a client software which can be used for telnet as well as for
@dfn{Multi-User Dungeon} (MUD).  Additionally it can serve as a nice client for
the chat server psyced with the specific config located at
http://lavachat.symlynx.com/unix/")
    (license license:gpl2+)))

(define-public red-eclipse
  (let ((release "1.6.0")
        (revision 0)
        (data-sources
         '(("acerspyro" "12b0bngl7hlxw4iwdbn99jp081yl6z1ic0s788nm349drbr2pck8")
           ("actors" "0x7qqx67679q6ark9zz02skwhzgabid69kwi6zmhfpfgicn4927r")
           ("appleflap" "08xslwqfqz3j4m03pv5ry2gdzj5k2ns51z8n6sln3sa94i9x8qkm")
           ("blendbrush" "18zf5i2ax4p14x4c9nhk9fq6l1xgbxw62gm72vx59vbfdpjrw3cg")
           ("caustics" "172fxwx7kbz5nmbjq98kr52ips505wb99fibgnpg8cj02syrya8k")
           ("crosshairs" "14w8ysqzdsx9bzpfbl700jzngbh14rdghhjdf6zd6jlkvrl6754r")
           ("dziq" "056imqszvp90j7cgz52ly0f31px64gsrmvm9k2c78ldbx87jnhc3")
           ("elyvisions" "1bsgr0gr7njydj8fqclh0a27lrsyic3xfd5a4vwggw7g54azpgk2")
           ("fonts" "00ibisza1qci0ghf2rynyf28l6r3nqhfzjf80k6gg76q4v7p1myx")
           ("freezurbern" "07l9ldk9b82f12c13wcg5xxdf15bw0yjxk3vvk8v3ygrl2mwksyr")
           ("john" "1jdmwkrdi5b9pivkm22rxhmkk1db9dx6l54wzcc23cvdz04ij93k")
           ("jojo" "0f7kjy43fbk9kw8fip6bbw4gn3pryh0fndlahjfkaysrx98krdj3")
           ("jwin" "0nc8dndnpqk2ad6316a8k6kgzsrkpwvk8s4gyh7aqfi4axfclril")
           ("luckystrike" "04jiipqahphmvz5cd74dygr62dlvv6l4iglb8hzh4pp8frhls8bq")
           ("maps" "0an46ipjvw4h0nxvb6qvnzp1cdkzlkiinqz4zh9lmxy1ds0gclim")
           ("mayhem" "15k10imm2wr6c6fq35n4r99k7kz7n9zdnjlw6dmdq6fba67i6sbc")
           ("mikeplus64" "0v4wiiivm3829j4phlavy22n4c6k6ib9ixxpdz7r6ysg5cdkaw33")
           ("misc" "13rfgwrlfhflz6inbkg3fypyf8im0m49sw112b17qrw2zgp8i1rz")
           ("nieb" "0z0h9jdn2gkkjil3vsvwidb1p2k09pi5n3wjxza12hhvqmcs7q8f")
           ("nobiax" "08bfp4q6gbfis18bp1h4d0hqssk79jc4fhyjxnv21dbam4v4mnkn")
           ("particles" "1vsx3fgg19xggxfhz3vlrh6nqhmw7kl9kmxrvb2j84blp00vd6z2")
           ("philipk" "14irscg80607i5k5l2ci0n9nwibvda2f3xsykgv96d0vldrp5n5a")
           ("projectiles" "09bnfsrywirwgjm6d7ff5nicx6w6b7w9568x9pb5q0ncaps3l85s")
           ("props" "1dlabbprlkif8af3daf9nbgcwgxiymvj0yiphqhlri8ylfy2vpz4")
           ("skyboxes" "14bi3md5y47cvb9ybipdvksz40gqsqw2r0lh3zzqb4acq367w18y")
           ("snipergoth" "0m8rvvy5n8n9pm0b5cqvzsxsw51mqk8m7s1h3qc849b38isliwq2")
           ("sounds" "0ivf3w5bphz5pzzx6kwcb67vbly1l19cgv3s0cyp8n87afiqj5rd")
           ("textures" "0qdmgx7zbcqnb9rrga2izr93p5inirczhddfxs504rsnv0v8vyxm")
           ("torley" "05ppyhghq859cbbxzj3dnl9fcx3ghy04ds1pylypwg2hsxzbjwcd")
           ("trak" "0g3vq86q91a3syli38lwc8ca4ychfwsmmqf85kqzfzyd627ybclm")
           ("ulukai" "0asa5fz400impklcg6dy2f7jiaqfc1sn1c36fpg8jd01gw66lw93")
           ("unnamed" "0rz5683j7sfwkcycfypbv4b0ihp0qwn9rzskfsabwc1s5g324917")
           ("vanities" "13f18783rc8cjf22p61zr8m5g1migzlx05fzl8xnbjdkqq4cdyix")
           ("vegetation" "1y5d97nfmvax7y4fr0y5v0c8zb1ajkqwx76kjd4qc9n4spdsi5sc")
           ("weapons" "103g1dhxv5ffz4ddg2xcbshbgv9606chsbas3pzk6h9ybqsyjrqh")
           ("wicked" "1884rk34a2dj83gz82rc4zh3ch0dyj5221hvsr0a5h60578i7yj6"))))
    (package
      (name "red-eclipse")
      (version (if (zero? revision)
                   release
                   (string-append release "-"
                                  (number->string revision))))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/red-eclipse/base.git")
               (commit (string-append "v" release))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qy9kmq21wc4bdhwifasxc5dv1y5c53sn7dfmyc5y3zyz8wjyij4"))
         (patches
          (search-patches "red-eclipse-remove-gamma-name-hack.patch"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f            ; no check target
         #:make-flags (list "CC=gcc" "-Csrc"
                            (string-append "INSTDIR="
                                           (assoc-ref %outputs "out") "/bin")
                            (string-append "prefix="
                                           (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-data
             (lambda* (#:key inputs #:allow-other-keys)
               (delete-file-recursively "data")
               (mkdir "data")
               (with-directory-excursion "data"
                 (for-each (lambda (name)
                             (copy-recursively (assoc-ref inputs name) name))
                           (list ,@(map car data-sources))))
               #t))
           (add-after 'unpack-data 'add-store-data-package-path-as-default
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "src/engine/server.cpp"
                 (("(else[[:space:]]*)((addpackagedir\\()\"data\"(\\);))"
                   _
                   else_part
                   addpackagedir_original
                   addpackagedir_open
                   addpackagedir_close)
                  (string-append else_part
                                 "{ "
                                 addpackagedir_open
                                 "\""
                                 (assoc-ref outputs "out")
                                 "/share/redeclipse/data\""
                                 addpackagedir_close
                                 " "
                                 addpackagedir_original
                                 " }")))
               #t))
           (delete 'configure)  ; no configure script
           (add-after 'set-paths 'set-sdl-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append (assoc-ref inputs "sdl-union")
                                      "/include/SDL2"))
               #t))
           (add-after 'install 'copy-data
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (copy-recursively "config"
                                   (string-append out "/config"))
                 (copy-file "doc/examples/servinit.cfg"
                            (string-append out "/config/servinit.cfg"))
                 (copy-recursively "data"
                                   (string-append out "/share/redeclipse/data"))
                 (mkdir-p (string-append out "/lib/redeclipse"))
                 (symlink (string-append out "/share/redeclipse/data")
                          (string-append out "/lib/redeclipse/data")))
               #t))
           (add-after 'copy-data 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (with-directory-excursion bin
                   (rename-file "redeclipse_linux"
                                ".redeclipse_linux-real")
                   (rename-file "redeclipse_server_linux"
                                ".redeclipse_server_linux-real")
                   (call-with-output-file "redeclipse_linux"
                     (lambda (port)
                       (format port "#!~a/bin/sh
# Run the thing from its home, otherwise it just bails out.
cd \"~a\"
exec -a \"$0\" ~a/.redeclipse_linux-real~%"
                               (assoc-ref inputs "bash") ;implicit input
                               (string-append out)
                               (string-append bin))))
                   (call-with-output-file "redeclipse_server_linux"
                     (lambda (port)
                       (format port "#!~a/bin/sh
# Run the thing from its home, otherwise it just bails out.
cd \"~a\"
exec -a \"$0\" ~a/.redeclipse_server_linux-real~%"
                               (assoc-ref inputs "bash") ;implicit input
                               (string-append out)
                               (string-append bin))))
                   (chmod "redeclipse_linux" #o555)
                   (chmod "redeclipse_server_linux" #o555)))
               #t)))))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("curl" ,curl)
         ("glu" ,glu)
         ("sdl-union" ,(sdl-union (list sdl2
                                        sdl2-image
                                        sdl2-mixer)))
         ;; Create origin records for the many separate data packages.
         ,@(map (match-lambda
                  ((name hash)
                   (list name
                         (origin
                           (method git-fetch)
                           (uri
                            (git-reference
                             (url (string-append "https://github.com/"
                                                 "red-eclipse/" name ".git"))
                             (commit (string-append "v" release))))
                           (sha256 (base32 hash))
                           (file-name (git-file-name name version))))))
                data-sources)))
      (home-page "http://redeclipse.net/")
      (synopsis "Arena shooter derived from the Cube 2 engine")
      (description
       "Red Eclipse is an arena shooter, created from the Cube2 engine.
Offering an innovative parkour system and distinct but all potent weapons,
Red Eclipse provides fast paced and accessible gameplay.")
      ;; The engine is under Zlib; data files are covered by the other
      ;; licenses.  More details at <http://redeclipse.net/wiki/License>.
      (license (list license:expat
                     license:zlib
                     license:cc-by-sa3.0
                     license:cc-by3.0
                     license:cc0)))))

(define-public grue-hunter
  (package
    (name "grue-hunter")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://jxself.org/grue-hunter.tar.gz"))
              (sha256
               (base32
                "1hjcpy5439qs3v2zykis7hsi0i17zjs62gks3zd8mnfw9ni4i2h3"))))
    (build-system trivial-build-system) ; no Makefile.PL
    (arguments `(#:modules ((guix build utils))
                 #:builder
                 (begin
                   (use-modules (guix build utils))
                   (use-modules (srfi srfi-1))

                   (let* ((tarball (assoc-ref %build-inputs "tarball"))
                          (perl    (string-append (assoc-ref %build-inputs
                                                             "perl")
                                                  "/bin"))
                          (gzip    (string-append (assoc-ref %build-inputs
                                                             "gzip")
                                                  "/bin/gzip"))
                          (tar     (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                          (out     (assoc-ref %outputs "out"))
                          (bin     (string-append out "/bin"))
                          (doc     (string-append out
                                                  "/share/doc/grue-hunter")))
                     (copy-file tarball "grue-hunter.tar.gz")
                     (invoke gzip "-d" "grue-hunter.tar.gz")
                     (invoke tar "xvf" "grue-hunter.tar")

                     (mkdir-p bin)
                     (copy-file "grue-hunter/gh.pl"
                                (string-append bin "/grue-hunter"))
                     (patch-shebang (string-append bin "/grue-hunter")
                                    (list perl))

                     (install-file "grue-hunter/AGPLv3.txt" doc)

                     #t))))
    (inputs `(("perl" ,perl)
              ("tar" ,tar)
              ("gzip" ,gzip)
              ("tarball" ,source)))
    (home-page "http://jxself.org/grue-hunter.shtml")
    (synopsis "Text adventure game")
    (description
     "Grue Hunter is a text adventure game written in Perl.  You must make
your way through an underground cave system in search of the Grue.  Can you
capture it and get out alive?")
    (license license:agpl3+)))

(define-public lierolibre
  (package
    (name "lierolibre")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/lierolibre/trunk/"
                                  version "/+download/lierolibre-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1cf1gvsn4qq190lrf9k5bpjnqwlcfw7pajvdnh7z5r4jqw0rsbl9"))
              (patches
               (search-patches "lierolibre-check-unaligned-access.patch"
                               "lierolibre-try-building-other-arch.patch"
                               "lierolibre-remove-arch-warning.patch"
                               "lierolibre-newer-libconfig.patch"
                               "lierolibre-is-free-software.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete pre-compiled files.
                  (delete-file "data/LIERO.CHR")
                  (delete-file "data/LIERO.SND")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("imagemagick" ,imagemagick)
       ("pkg-config" ,pkg-config)
       ("util-linux" ,util-linux)
       ("sox" ,sox)))
    (inputs
     `(("boost" ,boost)
       ("libconfig" ,libconfig)
       ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer)))
       ("zlib" ,zlib)))
    (home-page "https://gitlab.com/lierolibre/lierolibre")
    (synopsis "Old-school earthworm action game")
    (description
     "lierolibre is an earthworm action game where you fight another player
(or the computer) underground using a wide array of weapons.

Features:
@itemize
@item 2 worms, 40 weapons, great playability, two game modes: Kill'em All
and Game of Tag, plus AI-players without true intelligence!
@item Dat nostalgia.
@item Extensions via a hidden F1 menu:
@itemize
@item Replays
@item Game controller support
@item Powerlevel palettes
@end itemize
@item Ability to write game variables to plain text files.
@item Ability to load game variables from both EXE and plain text files.
@item Scripts to extract and repack graphics, sounds and levels.
@end itemize

To switch between different window sizes, use F6, F7 and F8, to switch to
fullscreen, use F5 or Alt+Enter.")
    ;; Code mainly BSD-2, some parts under Boost 1.0. All assets are WTFPL2.
    (license (list license:bsd-2 license:boost1.0 license:wtfpl2))))

(define-public warzone2100
  (package
    (name "warzone2100")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/warzone2100/archives/"
                           "unsupported/Warzone2100-"
                           (version-major+minor version) "/" version
                           "/warzone2100-" version ".tar.xz"))
       (sha256
        (base32 "10kmpr4cby95zwqsl1zwx95d9achli6khq7flv6xmrq30a39xazw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-distributor=Guix")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-tests-with-qt
           (lambda _
             (substitute* "tests/Makefile.in"
               (("(framework_linktest_LDADD|maptest_LDADD) = " prefix)
                (string-append prefix "$(QT5_LIBS) ")))
             #t))
         (add-after 'unpack 'fix-ivis-linktest
           (lambda _
             (substitute* "tests/ivis_linktest.cpp"
               (("iV_DrawTextRotated.*;")
                (string-append "iV_DrawTextRotated(\"Press ESC to exit.\", "
                               "100, 100, 0.0f, font_regular);")))
             #t)))))
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("pkg-config" ,pkg-config)
                     ("unzip" ,unzip)
                     ("zip" ,zip)))
    (inputs `(("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("fribidi" ,fribidi)
              ("glew" ,glew)
              ("harfbuzz" ,harfbuzz)
              ("libtheora" ,libtheora)
              ("libvorbis" ,libvorbis)
              ("libxrandr" ,libxrandr)
              ("openal" ,openal)
              ("physfs" ,physfs)
              ("qtbase" ,qtbase)
              ("qtscript" ,qtscript)
              ("openssl" ,openssl)
              ("sdl2" ,sdl2)))
    (home-page "http://wz2100.net")
    (synopsis "3D Real-time strategy and real-time tactics game")
    (description
     "Warzone 2100 offers campaign, multi-player, and single-player skirmish
modes. An extensive tech tree with over 400 different technologies, combined
with the unit design system, allows for a wide variety of possible units and
tactics.")
    ; Everything is GPLv2+ unless otherwise specified in COPYING.NONGPL
    (license (list license:bsd-3
                   license:cc0
                   license:cc-by-sa3.0
                   license:expat
                   license:gpl2+
                   license:lgpl2.1+))))

(define-public project-starfighter
  (package
    (name "project-starfighter")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/starfighter/"
                    (version-major+minor version) "/"
                    "starfighter-" version "-src.tar.gz"))
              (sha256
               (base32
                "1646hpjq8bz2fkfkja1dah511hn7zd2r7da4w9c9blhad3p5732v"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl2" ,sdl2)
       ("sdl2-image" ,sdl2-image)
       ("sdl2-mixer" ,sdl2-mixer)))
    (home-page "http://starfighter.nongnu.org/")
    (synopsis "2D scrolling shooter game")
    (description
     "In the year 2579, the intergalactic weapons corporation, WEAPCO, has
dominated the galaxy.  Guide Chris Bainfield and his friend Sid Wilson on
their quest to liberate the galaxy from the clutches of WEAPCO.  Along the
way, you will encounter new foes, make new allies, and assist local rebels
in strikes against the evil corporation.")
    ;; gfx and music are under CC-BY 3.0, CC-BY-SA 3.0, CC0 or Public Domain.
    (license (list license:gpl3+
                   license:cc-by3.0
                   license:cc-by-sa3.0
                   license:cc0
                   license:public-domain))))

(define-public starfighter
  (deprecated-package "starfighter" project-starfighter))

(define-public chromium-bsu
  (package
    (name "chromium-bsu")
    (version "0.9.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/chromium-bsu"
                                  "/Chromium B.S.U. source code/"
                                  "chromium-bsu-" version ".tar.gz"))
              (sha256
               (base32
                "0jk2w5b6s6nkzri585bbz16cif2fhqcnl5l1mq3rd98r9nil3hd1"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("gettext" ,gettext-minimal)
              ("glu" ,glu)
              ("quesoglc" ,quesoglc)
              ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer)))))
    (home-page "http://chromium-bsu.sourceforge.net/")
    (synopsis "Fast-paced, arcade-style, top-scrolling space shooter")
    (description
     "In this game you are the captain of the cargo ship Chromium B.S.U. and
are responsible for delivering supplies to the troops on the front line.  Your
ship has a small fleet of robotic fighters which you control from the relative
safety of the Chromium vessel.")
    ;; Clarified Artistic License for everything but sound, which is covered
    ;; by the Expat License.
    (license (list license:clarified-artistic license:expat))))

(define-public tuxpaint
  (package
    (name "tuxpaint")
    (version "0.9.23")                  ;keep VER_DATE below in sync
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint/"
                           version "/tuxpaint-" version ".tar.gz"))
       (sha256
        (base32
         "09k9pxi88r3dx6dyjwf9h85d4qpva4i29qz63dc558hg9v21k69l"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove win32 directory which contains binary dll's and the
           ;; deprecated visualc directory.
           (for-each delete-file-recursively '("win32" "visualc"))
           (substitute* "Makefile"
             ;; Do not rely on $(GPERF) being an absolute file name
             (("\\[ -x \\$\\(GPERF\\) \\]")
              "$(GPERF) --version >/dev/null 2>&1"))
           #t))
       (patches (search-patches "tuxpaint-stamps-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("fribidi" ,fribidi)
       ("gettext" ,gettext-minimal)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libpaper" ,libpaper)
       ("netpbm" ,netpbm)
       ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-ttf sdl-image)))))
    ;; TODO: Use system fonts rather than those in data/fonts
    (arguments
     `(#:make-flags `("VER_DATE=2018-09-02"
                      "GPERF=gperf" "CC=gcc"
                      "SDL_PCNAME=sdl SDL_image SDL_mixer SDL_ttf"
                      ,(string-append "PREFIX=" %output)
                      "KDE_PREFIX=$(PREFIX)/share/applications"
                      "KDE_ICON_PREFIX=$(PREFIX)/share/icons/"
                      "COMPLETIONDIR=$(PREFIX)/etc/bash_completion.d")
       #:parallel-build? #f             ;fails on some systems
       #:tests? #f                      ;No tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure phase
                  (add-before 'install 'no-sys-cache
                    (lambda _           ;do not rebuild system conf cache
                      (substitute* "Makefile"
                        (("kbuildsycoca4") ""))
                      #t))
                  (add-after 'install 'fix-import
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (net (assoc-ref inputs "netpbm"))
                             (tpi (string-append out "/bin/tuxpaint-import")))
                        (substitute* tpi
                          ;; Point to installation prefix so that the default
                          ;; configure file is found.
                          (("/usr/local") out))
                        ;; tuxpaint-import uses a bunch of programs from
                        ;; netpbm, so make sure it knows where those are
                        (wrap-program tpi
                          `("PATH" ":" prefix
                            (,(string-append net "/bin"))))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "TUXPAINT_STAMPS_PATH")
            (files '("share/tuxpaint/stamps")))))
    (home-page "http://www.tuxpaint.org")
    (synopsis "Drawing software for children")
    (description
     "Tux Paint is a free drawing program designed for young children (kids
ages 3 and up).  It has a simple, easy-to-use interface; fun sound effects;
and an encouraging cartoon mascot who helps guide children as they use the
program.  It provides a blank canvas and a variety of drawing tools to help
your child be creative.")
    (license license:gpl2+)))

(define-public tuxpaint-stamps
  (package
    (name "tuxpaint-stamps")
    (version "2018.09.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint-stamps/"
                           (string-map (λ (x) (if (eq? x #\.) #\- x)) version)
                           "/tuxpaint-stamps-" version ".tar.gz"))
       (sha256
        (base32
         "1skr23k27yj3vgwfazpzxp90lb2a278gxrkr3bxw7az6zpkmb3yp"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (setenv "PATH"
                           (string-append
                            (assoc-ref %build-inputs "tar") "/bin" ":"
                            (assoc-ref %build-inputs "gzip") "/bin"))
                   (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
                   (chdir (string-append ,name "-" ,version))
                   (let ((dir (string-append %output "/share/tuxpaint/stamps")))
                     (mkdir-p dir)
                     (copy-recursively "stamps" dir))
                   #t)))
    (home-page (package-home-page tuxpaint))
    (synopsis "Stamp images for Tux Paint")
    (description
     "This package contains a set of \"Rubber Stamp\" images which can be used
with the \"Stamp\" tool within Tux Paint.")
    (license license:gpl2+)))

(define-public tuxpaint-config
  (package
    (name "tuxpaint-config")
    (version "0.0.14")                  ;keep VER_DATE below in sync
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint-config/"
                           version "/tuxpaint-config-" version ".tar.gz"))
       (sha256
        (base32
         "0zkgxk436nqcp43zghkfmh397c7dvh5bwn2as7gwvv208bzyij6g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("fltk" ,fltk)
       ("libpaper" ,libpaper)
       ;; TODO: Should the following be propagated by fltk?
       ("libx11" ,libx11)
       ("libxft" ,libxft)
       ("mesa" ,mesa)))
    (arguments
     `(#:make-flags `("VER_DATE=2018-09-01"
                      "CONFDIR=/etc/tuxpaint" ;don't write to store
                      ,(string-append "PREFIX=" %output)
                      "GNOME_PREFIX=$(PREFIX)")
       #:parallel-build? #f             ;race conditions
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure phase
                  (add-before 'install 'gzip-no-name
                    (lambda* _
                      (substitute* "Makefile"
                        ;; tuxpaint-config compresses its own documentation;
                        ;; make sure it uses flags for reproducibility.
                        (("gzip") "gzip --no-name"))))
                  (add-before 'install 'make-install-dirs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        #t))))))
    (home-page (package-home-page tuxpaint))
    (synopsis "Configure Tux Paint")
    (description
     "Tux Paint Config is a graphical configuration editor for Tux Paint.")
    (license license:gpl2)))            ;no "or later" present

(define-public supertux
  (package
   (name "supertux")
   (version "0.5.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/SuperTux/supertux/"
                                "releases/download/v" version "/SuperTux-v"
                                version "-Source.tar.gz"))
            (sha256
             (base32
              "1i8avad7w7ikj870z519j383ldy29r6f956bs38cbr8wk513pp69"))))
   (arguments
    '(#:tests? #f
      #:configure-flags '("-DINSTALL_SUBDIR_BIN=bin"
                          "-DENABLE_BOOST_STATIC_LIBS=OFF")))
   (build-system cmake-build-system)
   (inputs `(("sdl2" ,sdl2)
             ("sdl2-image" ,sdl2-image)
             ("sdl2-mixer" ,sdl2-mixer)
             ("openal" ,openal)
             ("mesa" ,mesa)
             ("glew" ,glew)
             ("libvorbis" ,libvorbis)
             ("libogg" ,libogg)
             ("physfs" ,physfs)
             ("curl" ,curl)
             ("boost" ,boost)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (synopsis "2D platformer game")
   (description "SuperTux is a free classic 2D jump'n run sidescroller game
in a style similar to the original Super Mario games covered under
the GNU GPL.")
   (home-page "https://supertuxproject.org/")
   (license license:gpl3+)))

(define-public tintin++
  (package
    (name "tintin++")
    (version "2.01.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tintin/TinTin++ Source Code/"
                           (version-major+minor version)
                           "/tintin-" version ".tar.gz"))
       (sha256
        (base32
         "033n84pyxml3n3gd4dq0497n9w331bnrr1gppwipz9ashmq8jz7v"))))
    (inputs
     `(("gnutls" ,gnutls)
       ("pcre" ,pcre)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (arguments
     '(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         ;; The source is in tt/src.
         (add-before 'configure 'chdir
           (lambda _
             (chdir "src")
             #t)))))
    (build-system gnu-build-system)
    (home-page "http://tintin.sourceforge.net/")
    (synopsis "MUD client")
    (description
     "TinTin++ is a MUD client which supports MCCP (Mud Client Compression
Protocol), MMCP (Mud Master Chat Protocol), xterm 256 colors, most TELNET
options used by MUDs, as well as those required to login via telnet on
Linux / Mac OS X servers, and an auto mapper with a VT100 map display.")
    (license license:gpl2+)))

(define-public laby
  (package
    (name "laby")
    (version "0.6.4")
    (source
     (origin (method url-fetch)
             (uri (string-append
                   "https://github.com/sgimenez/laby/archive/"
                   "laby-" version ".tar.gz"))
             (sha256
              (base32
               "0gyrfa95l1qka7gbjf7l6mk7mbfvph00l0c995ia272qdw7rjhyf"))
             (patches (search-patches "laby-make-install.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("lablgtk" ,lablgtk)
       ("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib)
       ("ocamlbuild" ,ocamlbuild)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'allow-unsafe-strings
           ;; Fix a build failure with ocaml >=4.06.0.
           ;; See <https://github.com/sgimenez/laby/issues/53>.
           (lambda _
             (setenv "OCAMLPARAM" "safe-string=0,_")
             #t))
         (add-before 'build 'set-library-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lablgtk (assoc-ref inputs "lablgtk")))
               (setenv "LD_LIBRARY_PATH"
                       (string-append lablgtk "/lib/ocaml/stublibs"))))))
       #:tests? #f ; no 'check' target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")) "all")))
    (home-page "https://sgimenez.github.io/laby/")
    (synopsis "Programming game")
    (description "Learn programming, playing with ants and spider webs ;-)
Your robot ant can be programmed in many languages: OCaml, Python, C, C++,
Java, Ruby, Lua, JavaScript, Pascal, Perl, Scheme, Vala, Prolog.  Experienced
programmers may also add their own favorite language.")
    (license license:gpl3+)))

(define-public bambam
  (package
    (name "bambam")
    (version "0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/porridge/bambam")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "08hcd0gzia3pz7fzk4pqc5kbq1074j4q0jcmbpgvr7n623nj2xa5"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (add-before 'install 'patch-data-dir-location
           (lambda _
             (substitute* "bambam.py"
               (("'data'") "'../share/bambam/data'"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share")))
               (mkdir-p bin)
               (copy-file "bambam.py" (string-append bin "/bambam"))
               (install-file "bambam.6" (string-append share "/man/man6"))
               (copy-recursively "data" (string-append share "/bambam/data")))
             #t)))))
    (inputs
     `(("python-pygame" ,python-pygame)))
    (home-page "https://github.com/porridge/bambam")
    (synopsis "Keyboard mashing and doodling game for babies")
    (description "Bambam is a simple baby keyboard (and gamepad) masher
application that locks the keyboard and mouse and instead displays bright
colors, pictures, and sounds.")
    (license license:gpl3+)))

(define-public mrrescue
  (package
    (name "mrrescue")
    (version "1.02e")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/SimonLarsen/mrrescue/releases/"
                    "download/" version "/mrrescue" version ".love"))
              (file-name (string-append name "-" version ".love"))
              (sha256
               (base32
                "0jwzbwkgp1l5ia6c7s760gmdirbsncp6nfqp7vqdqsfb63la9gl2"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (script (string-append out "/bin/" ,name))
                (data   (string-append out "/share/" ,name))
                (source (assoc-ref %build-inputs "source"))
                (unzip  (string-append (assoc-ref %build-inputs "unzip")
                                       "/bin/unzip"))
                (patch  (string-append (assoc-ref %build-inputs "patch")
                                       "/bin/patch"))
                (bash   (string-append (assoc-ref %build-inputs "bash")
                                       "/bin/bash"))
                (love   (string-append (assoc-ref %build-inputs "love")
                                       "/bin/love")))

           (mkdir-p (dirname script))
           (with-output-to-file script
             (lambda ()
               (format #t "#!~a~%" bash)
               (format #t "exec -a ~a \"~a\" \"~a\"~%" ,name love data)))
           (chmod script #o755)

           ;; The better way to package this game would be to install *only* the
           ;; script above, pointing to the unextracted .love file in the store.
           ;; However, mrrescue 1.02e needs to be patched to work with Love 11.
           ;; Instead of extracting the .love file, patching it, and re-zipping
           ;; it to the store, simply point the script to the extracted patched
           ;; data directory directly.
           (mkdir-p data)
           (with-directory-excursion data
             (invoke unzip source)
             (invoke patch "-p1" "-i"
                     (assoc-ref %build-inputs "love-11.patch")))
           #t))))
    (native-inputs
     `(("unzip" ,unzip)
       ("patch" ,patch)
       ("love-11.patch" ,(search-patch "mrrescue-support-love-11.patch"))))
    (inputs
     `(("bash" ,bash)
       ("love" ,love)))
    (home-page "http://tangramgames.dk/games/mrrescue")
    (synopsis "Arcade-style fire fighting game")
    (description
     "Mr. Rescue is an arcade styled 2d action game centered around evacuating
civilians from burning buildings.  The game features fast-paced fire
extinguishing action, intense boss battles, a catchy soundtrack, and lots of
throwing people around in pseudo-randomly generated buildings.")
    (license (list license:zlib             ; for source code
                   license:cc-by-sa3.0))))  ; for graphics and music assets

(define-public hyperrogue
  (package
    (name "hyperrogue")
    (version "10.5d")
    ;; When updating this package, be sure to update the "hyperrogue-data"
    ;; origin in native-inputs.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.roguetemple.com/z/hyper/hyperrogue"
                    (string-join (string-split version #\.) "")
                    "-src.tgz"))
              (sha256
               (base32
                "1ls055v4pv2xmn2a8lav7wl370zn0wsd91q41bk0amxd168kcndy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags '("HYPERROGUE_USE_GLEW=1"
                      "HYPERROGUE_USE_PNG=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl-union")
                                    "/include/SDL"))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share-dir (string-append out "/share/hyperrogue"))
                    (dejavu-dir (string-append
                                 (assoc-ref inputs "font-dejavu")
                                 "/share/fonts/truetype"))
                    (dejavu-font "DejaVuSans-Bold.ttf")
                    (music-file "hyperrogue-music.txt"))
               ;; Fix font and music paths.
               (substitute* "basegraph.cpp"
                 ((dejavu-font)
                  (string-append dejavu-dir "/" dejavu-font)))
               (substitute* music-file
                 (("\\*/")
                  (string-append share-dir "/sounds/")))
               (substitute* "sound.cpp"
                 (("musicfile = \"\"")
                  (string-append "musicfile = \""
                                 share-dir "/" music-file "\"")))
               ;; Disable build machine CPU optimizations and warnings treated
               ;; as errors.
               (substitute* "Makefile"
                 (("-march=native") "")
                 (("-Werror") "")))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share-dir (string-append out "/share/hyperrogue")))
               (mkdir-p bin)
               (install-file "hyperrogue" bin)
               (install-file "hyperrogue-music.txt" share-dir))
             #t))
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((data (assoc-ref inputs "hyperrogue-data"))
                    (out (assoc-ref outputs "out"))
                    (sounds (string-append out "/share/hyperrogue/sounds"))
                    (unzip (string-append (assoc-ref inputs "unzip") "/bin/unzip")))
               ;; Extract media license information into sounds directory.
               (invoke unzip "-j" data
                       (string-append
                        "hyperrogue"
                        (string-join (string-split ,version #\.) "")
                        "/sounds/credits.txt") "-d" sounds)
               ;; Extract sounds and music into sounds directory.
               (invoke "unzip" "-j" data
                       (string-append
                        "hyperrogue"
                        (string-join (string-split ,version #\.) "")
                        "/*.ogg") "-d" sounds)))))))
    (native-inputs
     `(("hyperrogue-data"
        ,(origin
           (method url-fetch)
           (uri
            (string-append
             "https://www.roguetemple.com/z/hyper/hyperrogue"
             (string-join (string-split version #\.) "")
             "-win.zip"))
           (sha256
            (base32
             "13n9hcvf9yv7kjghm5jhjpwq1kh94i4bgvcczky9kvdvw1y9278n"))))
       ("unzip" ,unzip)))
    (inputs
     `(("font-dejavu" ,font-dejavu)
       ("glew" ,glew)
       ("libpng" ,libpng)
       ("sdl-union" ,(sdl-union (list sdl
                                      sdl-gfx
                                      sdl-mixer
                                      sdl-ttf)))))
    (home-page "https://www.roguetemple.com/z/hyper/")
    (synopsis "Non-euclidean graphical rogue-like game")
    (description
     "HyperRogue is a game in which the player collects treasures and fights
monsters -- rogue-like but for the fact that it is played on the hyperbolic
plane and not in euclidean space.

In HyperRogue, the player can move through different parts of the world, which
are home to particular creatures and may be subject to their own rules of
\"physics\".

While the game can use ASCII characters to display the the classical rogue
symbols, it still needs graphics to render the non-euclidean world.")
    (license (list license:bsd-3         ; glew.c, mtrand.*
                   license:cc-by-sa3.0   ; music
                   license:cc-by-sa4.0   ; sounds
                   license:cc0
                   license:public-domain ; direntx.*, some sounds
                   license:zlib          ; savepng.*
                   license:gpl2+))))     ; remaining files

(define-public kobodeluxe
  (package
    (name "kobodeluxe")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://olofson.net/kobodl/download/KoboDeluxe-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0b2wvdpnmaibsy419c16dfwj5kvd3pccby2aaqvm964x74592yqg"))
              (patches (search-patches
                        "kobodeluxe-const-charp-conversion.patch"
                        "kobodeluxe-enemies-pipe-decl.patch"
                        "kobodeluxe-graphics-window-signed-char.patch"
                        "kobodeluxe-manpage-minus-not-hyphen.patch"
                        "kobodeluxe-midicon-segmentation-fault.patch"
                        "kobodeluxe-paths.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))))
    (inputs `(("sdl-union" ,(sdl-union (list sdl sdl-image)))))
    (synopsis "Shooter with space station destruction")
    (description
     "Kobo Deluxe is an enhanced version of Akira Higuchi's XKobo graphical game
for Un*x systems with X11.")
    (home-page "http://olofson.net/kobodl/")
    (license license:gpl2+)))

(define-public freeciv
  (package
   (name "freeciv")
   (version "2.6.0")
   (source
    (origin
     (method url-fetch)
     (uri (list (string-append
                  "http://files.freeciv.org/stable/freeciv-"
                  version ".tar.bz2")
                (string-append
                  "mirror://sourceforge/freeciv/Freeciv%20"
                  (version-major+minor version) "/" version
                  "/freeciv-" version ".tar.bz2")))
     (sha256
      (base32
       "16f9wsnn7073s6chzbm3819swd0iw019p9nrzr3diiynk28kj83w"))))
   (build-system gnu-build-system)
   (inputs
    `(("curl" ,curl)
      ("cyrus-sasl" ,cyrus-sasl)
      ("gtk+" ,gtk+)
      ("sdl-mixer" ,sdl-mixer)
      ("zlib" ,zlib)))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (home-page "http://www.freeciv.org/")
   (synopsis "Turn based empire building strategy game")
   (description "Freeciv is a turn based empire building strategy game
inspired by the history of human civilization.  The game commences in
prehistory and your mission is to lead your tribe from the Stone Age
to the Space Age.")
   (license license:gpl2+)))

(define-public no-more-secrets
  (package
    (name "no-more-secrets")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bartobri/no-more-secrets.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zfv4qabikf8w9winsr4brxrdvs3f0d7xvydksyx8bydadsm2v2h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list "CC=gcc" "all-ncurses"
                          (string-append "prefix="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://github.com/bartobri/no-more-secrets")
    (synopsis "Recreation of data decryption effect in \"Sneakers\"")
    (description
     "@code{No More Secrets} provides a command line tool called \"nms\"
that recreates the famous data decryption effect seen on screen in the 1992
movie \"Sneakers\".

This command works on piped data.  Pipe any ASCII or UTF-8 text to nms, and
it will apply the hollywood effect, initially showing encrypted data, then
starting a decryption sequence to reveal the original plaintext characters.")
    (license license:expat)))

(define-public megaglest-data
  (package
    (name "megaglest-data")
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/MegaGlest/megaglest-data"
             "/releases/download/" version "/megaglest-data-"
             version ".tar.xz"))
       (sha256
        (base32
         "0ipgza33z89fw3si32iafm981f3fvm0zldvbxj29whghd2k3rpj3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://megaglest.org/")
    (synopsis "Data files for MegaGlest")
    (description "This package contains the data files required for MegaGlest.")
    (license license:cc-by-sa3.0)))

(define-public megaglest
  (package
    (name "megaglest")
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/MegaGlest/megaglest-source"
             "/releases/download/" version "/megaglest-source-"
             version ".tar.xz"))
       (sha256
        (base32
         "1ffck3ii1wp5k3nn5p0ga06jgp7pzk4zw0xln3xim2w7qrxzdzh9"))))
    (build-system cmake-build-system)
    (inputs
     `(("curl" ,curl)
       ("fontconfig" ,fontconfig)
       ("ftgl" ,ftgl)
       ("glew" ,glew)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("megaglest-data" ,megaglest-data)
       ("mesa" ,mesa)
       ("miniupnpc" ,miniupnpc)
       ("openal" ,openal)
       ("libircclient" ,libircclient)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("lua" ,lua)
       ("sdl2" ,sdl2)
       ("wxwidgets" ,wxwidgets)))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "-DCUSTOM_DATA_INSTALL_PATH="
                            (assoc-ref %build-inputs "megaglest-data")
                            "/share/megaglest")
             "-DBUILD_MEGAGLEST_TESTS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-ini-search-path
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "source/glest_game/global/config.cpp"
                        (("/usr/share/megaglest/")
                         (string-append (assoc-ref outputs "out")
                                        "/share/megaglest/"))))))
       #:test-target "megaglest_tests"))
    (home-page "https://megaglest.org/")
    (synopsis "3D real-time strategy (RTS) game")
    (description "MegaGlest is a cross-platform 3D real-time strategy (RTS)
game, where you control the armies of one of seven different factions: Tech,
Magic, Egypt, Indians, Norsemen, Persian or Romans.")
    (license license:gpl2+)))

(define-public freegish
  (let ((commit "8795cd7adc95957883f2d3465eb9036a774667a7")
        (revision "1"))
    (package
      (name "freegish")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/freegish/freegish.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1p1zf5qqagmcpi1db2bs02cnalpy3qiymp6yzan7k1bhmv859gsx"))
                (modules '((guix build utils)))
                ;; The audio files in the "music" directory are licensed under
                ;; CC-BY-NC, so we delete them.
                (snippet
                 '(begin
                    (delete-file-recursively "music")
                    #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ; no tests included
         #:configure-flags
         (list "-DCMAKE_INSTALL_FHS=ON")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'set-DATAPATH
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "CMakeLists.txt"
                 (("^option\\(INSTALL_FHS" line)
                  (string-append "add_definitions(-DDATAPATH=\""
                                 (assoc-ref outputs "out") "/share/freegish\")\n"
                                 line)))
               #t)))))
      (inputs
       `(("sdl-union" ,(sdl-union (list sdl sdl-mixer)))
         ("openal" ,openal)
         ("libvorbis" ,libvorbis)
         ("libogg" ,libogg)
         ("mesa" ,mesa)
         ("libpng" ,libpng)
         ("zlib" ,zlib)))
      (home-page "https://github.com/freegish/freegish")
      (synopsis "Side-scrolling physics platformer with a ball of tar")
      (description "In FreeGish you control Gish, a ball of tar who lives
happily with his girlfriend Brea, until one day a mysterious dark creature
emerges from a sewer hole and pulls her below ground.")
      ;; The textures are available under the Expat license.  All other assets
      ;; (including levels) are covered under CC-BY-SA or public domain.  The
      ;; source code is under GPLv2+.
      (license (list license:gpl2+
                     license:expat
                     license:public-domain
                     license:cc-by-sa3.0)))))

(define-public cdogs-sdl
  (package
    (name "cdogs-sdl")
    (version "0.6.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cxong/cdogs-sdl.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13gyv2hzk43za1n3lsjnd5v64xlzfzq7n10scd1rcbsnk1n007zr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCDOGS_DATA_DIR="
                            (assoc-ref %outputs "out")
                            "/share/cdogs-sdl/"))))
    (inputs
     `(("mesa" ,mesa)
       ("sdl2" ,sdl2)
       ("sdl2-image" ,sdl2-image)
       ("sdl2-mixer" ,sdl2-mixer)))
    (home-page "https://cxong.github.io/cdogs-sdl/")
    (synopsis "Classic overhead run-and-gun game")
    (description "C-Dogs SDL is a classic overhead run-and-gun game,
supporting up to 4 players in co-op and deathmatch modes.  Customize your
player, choose from many weapons, and blast, slide and slash your way through
over 100 user-created campaigns.")
    ;; GPLv2+ for code (includes files under BSD-2 and BSD-3),
    ;; CC0/CC-BY/CC-BY-SA for assets.
    (license (list license:gpl2+
                   license:bsd-2
                   license:bsd-3
                   license:cc0
                   license:cc-by3.0
                   license:cc-by-sa3.0))))

(define-public kiki-the-nano-bot
  (package
    (name "kiki-the-nano-bot")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/kiki/kiki-src/"
                                  version "/kiki-" version "-src.tgz"))
              (sha256
               (base32
                "0ihjdsxbn8z3cz0gpcprafiipcqaiskgdnh1rhmw4qff8dszalbn"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.dll$"))
                  #t))
              (patches
               (search-patches "kiki-level-selection-crash.patch"
                               "kiki-makefile.patch"
                               "kiki-missing-includes.patch"
                               "kiki-portability-64bit.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags '("CXX=g++")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "sdl-union")
                                    "/include/SDL:"
                                    (assoc-ref inputs "python")
                                    "/include/python2.7:"
                                    (getenv "CPLUS_INCLUDE_PATH")))
             (substitute* "src/main/main.cpp"
               (("#include <SDL.h>" line)
                (string-append line "
#define K_INCLUDE_GLUT
#include \"KIncludeTools.h\""))
               (("// initialize SDL" line)
                (string-append "glutInit(&argc,argv);\n" line)))
             (substitute* "src/main/KikiController.cpp"
               (("getenv\\(\"KIKI_HOME\"\\)")
                (string-append "\"" (assoc-ref outputs "out") "/share/kiki/\"")))
             (substitute* "linux/Makefile"
               (("CXXOPTS =" line)
                (string-append line " -fpermissive"))
               (("PYTHON_VERSION=.*") "PYTHON_VERSION=2.7")
               (("PYTHONHOME =.*")
                (string-append "PYTHONHOME = "
                               (assoc-ref inputs "python")
                               "/lib/python2.7/"))
               (("\\$\\(GLLIBS\\)" line)
                (string-append line " -lm -lpython2.7")))
             (substitute* "src/main/KikiPythonWidget.h"
               (("#define __KikiPythonWidget" line)
                (string-append line "\n#include \"KikiPython.h\"")))
             #t))
         (add-before 'build 'build-kodilib
           (lambda* (#:key make-flags #:allow-other-keys)
             (with-directory-excursion "kodilib/linux"
               (apply invoke "make" make-flags))))
         (add-after 'build-kodilib 'chdir
           (lambda _ (chdir "linux") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share/kiki")))
               (mkdir-p bin)
               (mkdir-p share)
               (install-file "kiki" bin)
               (copy-recursively "../py" (string-append share "/py"))
               (copy-recursively "../sound" (string-append share "/sound"))
               #t))))))
    (inputs
     `(("glu" ,glu)
       ;; Kiki builds fine with freeglut 3.0.0 but segfaults on start.
       ("freeglut" ,freeglut-2.8)
       ("sdl-union" ,(sdl-union (list sdl
                                      sdl-mixer
                                      sdl-image)))
       ("python" ,python-2)))
    (native-inputs
     `(("swig" ,swig)))
    (home-page "http://kiki.sourceforge.net/")
    (synopsis "3D puzzle game")
    (description "Kiki the nano bot is a 3D puzzle game.  It is basically a
mixture of the games Sokoban and Kula-World.  Your task is to help Kiki, a
small robot living in the nano world, repair its maker.")
    ;; See <http://metadata.ftp-master.debian.org/changelogs/main/k/
    ;; kiki-the-nano-bot/kiki-the-nano-bot_1.0.2+dfsg1-4_copyright>
    ;; for a statement from the author.
    (license license:public-domain)))

(define-public kiki
  (deprecated-package "kiki" kiki-the-nano-bot))

(define-public teeworlds
  (package
    (name "teeworlds")
    (version "0.7.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/teeworlds/teeworlds.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15l988qcsqgb6rjais0qd5sd2rjanm2708jmzvkariqzz0d6pb93"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (ice-9 regex)
                         (srfi srfi-1)
                         (srfi srfi-26)))
              (snippet ; remove bundled libraries except md5
               '(let ((base-dir "src/engine/external/"))
                  (for-each (compose (cut delete-file-recursively <>)
                                     (cut string-append base-dir <>))
                            (remove (cut string-match "(^.)|(^md5$)" <>)
                                    (scandir base-dir)))
                  #t))
              (patches
               (search-patches "teeworlds-use-latest-wavpack.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The bundled json-parser uses an old API.
             ;; To use the latest non-bundled version, we need to pass the
             ;; length of the data in all 'json_parse_ex' calls.
             (define (use-latest-json-parser file)
               (substitute* file
                 (("engine/external/json-parser/json\\.h")
                  "json-parser/json.h")
                 (("json_parse_ex\\(&JsonSettings, pFileData, aError\\);")
                  "json_parse_ex(&JsonSettings,
                                 pFileData,
                                 strlen(pFileData),
                                 aError);")))

             ;; Embed path to assets.
             (substitute* "src/engine/shared/storage.cpp"
               (("#define DATA_DIR.*")
                (string-append "#define DATA_DIR \""
                               (assoc-ref outputs "out")
                               "/share/teeworlds/data"
                               "\"")))

             ;; Bam expects all files to have a recent time stamp.
             (for-each (cut utime <> 1 1)
                       (find-files "."))

             ;; Do not use bundled libraries.
             (substitute* "bam.lua"
               (("local json = Compile.+$")
                "local json = nil
settings.link.libs:Add(\"jsonparser\")")
               (("local png = Compile.+$")
                "local png = nil
settings.link.libs:Add(\"pnglite\")")
               (("local wavpack = Compile.+$")
                "local wavpack = nil
settings.link.libs:Add(\"wavpack\")")
               (("if config\\.zlib\\.value == 1")
                "if config.zlib.value"))
             (substitute* "src/engine/client/graphics_threaded.cpp"
               (("engine/external/pnglite/pnglite\\.h")
                "pnglite.h"))
             (substitute* "src/engine/client/sound.cpp"
               (("engine/external/wavpack/wavpack\\.h")
                "wavpack/wavpack.h"))
             (for-each use-latest-json-parser
                       '("src/game/client/components/countryflags.cpp"
                         "src/game/client/components/menus_settings.cpp"
                         "src/game/client/components/skins.cpp"
                         "src/game/client/localization.cpp"
                         "src/game/editor/auto_map.h"
                         "src/game/editor/editor.cpp"))
             #t))
         (replace 'build
           (lambda _
             (invoke "bam" "-a" "-v" "conf=release")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((arch ,(system->linux-architecture
                            (or (%current-target-system)
                                (%current-system))))
                    (build (string-append "build/" arch "/release/"))
                    (data-built (string-append build "data/"))
                    (out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (data (string-append out "/share/teeworlds/data/")))
               (for-each (cut install-file <> bin)
                         (map (cut string-append build <>)
                              '("teeworlds" "teeworlds_srv")))
               (copy-recursively data-built data)
               #t))))))
    (inputs
     `(("freetype" ,freetype)
       ("glu" ,glu)
       ("json-parser" ,json-parser)
       ("mesa" ,mesa)
       ("pnglite" ,pnglite)
       ("sdl2" ,sdl2)
       ("sdl2-image" ,sdl2-image)
       ("sdl2-mixer" ,sdl2-mixer)
       ("wavpack" ,wavpack)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bam" ,bam)
       ("python" ,python-wrapper)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.teeworlds.com")
    (synopsis "2D retro multiplayer shooter game")
    (description "Teeworlds is an online multiplayer game.  Battle with up to
16 players in a variety of game modes, including Team Deathmatch and Capture
The Flag.  You can even design your own maps!")
    (license license:bsd-3)))

(define-public enigma
  (package
    (name "enigma")
    (version "1.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/enigma-game/"
                                  "Release%20" version "/enigma-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00ffh9pypj1948pg3q9sjp1nmiabh52p5c8wpg9n1dcfgl3cywnq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-system-enet")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-sdl
           (lambda _
             (substitute* "configure"
               (("SDL_ttf.h") "SDL/SDL_ttf.h"))
             (substitute* '("tools/ttf2bmf.cc"
                            "lib-src/enigma-core/ecl_font.cc"
                            "lib-src/enigma-core/ecl_video.cc"
                            "lib-src/enigma-core/ecl_buffer.hh"
                            "src/SoundEngine.cc"
                            "src/SoundEngine.hh"
                            "src/MusicManager.cc"
                            "src/MusicManager.hh"
                            "src/d_models.cc"
                            "src/main.cc"
                            "src/network.cc")
               (("#include \"SDL_(image|ttf|mixer|types|syswm|mutex).h\"" line header)
                (string-append "#include \"SDL/SDL_" header ".h\"")))
             (substitute* "src/main.cc"
               (("#include <SDL_(image|ttf|mixer).h>" line header)
                (string-append "#include \"SDL/SDL_" header ".h\"")))
             #t)))))
    (inputs
     `(("xerces-c" ,xerces-c)
       ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-ttf)))
       ("curl" ,curl)
       ("enet" ,enet)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("imagemagick" ,imagemagick)))
    (home-page "https://www.nongnu.org/enigma")
    (synopsis "Puzzle game with a dexterity component")
    (description "Enigma is a puzzle game with 550 unique levels.  The object
of the game is to find and uncover pairs of identically colored ‘Oxyd’ stones.
Simple?  Yes.  Easy?  Certainly not!  Hidden traps, vast mazes, laser beams,
and most of all, countless hairy puzzles usually block your direct way to the
Oxyd stones.  Enigma’s game objects (and there are hundreds of them, lest you
get bored) interact in many unexpected ways, and since many of them follow the
laws of physics (Enigma’s special laws of physics, that is), controlling them
with the mouse isn’t always trivial.")
    (license license:gpl2+)))

(define-public chroma
  (package
    (name "chroma")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://level7.org.uk/chroma/download/chroma-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1gfaw1kf7cxf5ibr61n6dxjihi49gmysn4cvawx1pqvy52ljpk0p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests included
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-ttf)))
       ("freetype" ,freetype)
       ("ncurses" ,ncurses)
       ("fontconfig" ,fontconfig)
       ("libxft" ,libxft)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://level7.org.uk/chroma/")
    (synopsis "Abstract puzzle game")
    (description "Chroma is an abstract puzzle game. A variety of colourful
shapes are arranged in a series of increasingly complex patterns, forming
 fiendish traps that must be disarmed and mysterious puzzles that must be
 manipulated in order to give up their subtle secrets. Initially so
 straightforward that anyone can pick it up and begin to play, yet gradually
 becoming difficult enough to tax even the brightest of minds.")
    (license license:gpl2+)))

(define-public fish-fillets-ng
  (package
    (name "fish-fillets-ng")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/fillets/"
                                  "Fish%20Fillets%20-%20Next%20Generation/"
                                  version "/fillets-ng-" version ".tar.gz"))
              (sha256
               (base32
                "1nljp75aqqb35qq3x7abhs2kp69vjcj0h1vxcpdyn2yn2nalv6ij"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-lua="
                            (assoc-ref %build-inputs "lua")))
       #:make-flags
       (list (string-append "CFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL")
             (string-append "CXXFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))
       #:phases
       (modify-phases %standard-phases
         ;; Lua 5.1 does not provide it.
         (add-after 'unpack 'do-not-link-with-lualib
           (lambda _
             (substitute* "configure"
               (("-llualib") ""))
             #t))
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((data (string-append (assoc-ref outputs "out")
                                        "/share/games/fillets-ng")))
               (mkdir-p data)
               (invoke "tar" "-xvf"
                       (assoc-ref inputs "fillets-ng-data")
                       "--strip-components=1"
                       "-C" data)))))))
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl
                                      sdl-mixer
                                      sdl-image
                                      sdl-ttf)))
       ("fribidi" ,fribidi)
       ("libx11" ,libx11)
       ("lua" ,lua-5.1)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("fillets-ng-data"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://sourceforge/fillets/"
                               "Fish%20Fillets%20-%20Next%20Generation/"
                               version "/fillets-ng-data-" version ".tar.gz"))
           (sha256
            (base32
             "169p0yqh2gxvhdilvjc2ld8aap7lv2nhkhkg4i1hlmgc6pxpkjgh"))))))
    (home-page "http://fillets.sourceforge.net/")
    (synopsis "Puzzle game")
    (description "Fish Fillets NG is strictly a puzzle game.  The goal in
every of the seventy levels is always the same: find a safe way out.  The fish
utter witty remarks about their surroundings, the various inhabitants of their
underwater realm quarrel among themselves or comment on the efforts of your
fish.  The whole game is accompanied by quiet, comforting music.")
    (license license:gpl2+)))

(define-public fillets-ng
  (deprecated-package "fillets-ng" fish-fillets-ng))

(define-public dungeon-crawl-stone-soup
  (package
    (name "dungeon-crawl-stone-soup")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (list
             ;; Older releases get moved into a versioned directory
             (string-append "http://crawl.develz.org/release/"
                            (version-major+minor version) "/stone_soup-"
                            version "-nodeps.tar.xz")
             ;; Only the latest release is in this directory
             (string-append "http://crawl.develz.org/release/stone_soup-"
                            version "-nodeps.tar.xz")))
       (sha256
        (base32
         "0c3mx49kpz6i2xvv2dwsaj9s7mm4mif1h2qdkfyi80lv2j1ay51h"))
       (patches (search-patches "crawl-upgrade-saves.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("lua51" ,lua-5.1)
       ("ncurses" ,ncurses)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("perl" ,perl)
       ("python" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:make-flags
       (let* ((sqlite (assoc-ref %build-inputs "sqlite"))
              (out (assoc-ref %outputs "out")))
         (list (string-append "SQLITE_INCLUDE_DIR=" sqlite "/include")
               (string-append "prefix=" out)
               "SAVEDIR=~/.crawl"
               ;; Don't compile with SSE on systems which don't use it
               ,@(match (%current-system)
                   ((or "i686-linux" "x86_64-linux")
                    '())
                   (_ '("NOSSE=TRUE")))
               ;; don't build any bundled dependencies
               "BUILD_LUA="
               "BUILD_SQLITE="
               "BUILD_ZLIB="
               "-Csource"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-SDL-image
           (lambda _
             (substitute* "source/windowmanager-sdl.cc"
               (("SDL_image.h") "SDL2/SDL_image.h"))
             #t))
         (delete 'configure)
         (replace 'check
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
             (setenv "HOME" (getcwd))
             ;; Fake a terminal for the test cases.
             (setenv "TERM" "xterm-256color")
             ;; Run the tests that don't require a debug build.
             (apply invoke "make" "nondebugtest"
                    (format #f "-j~d" (parallel-job-count))
                    ;; Force command line build for test cases.
                    (append make-flags '("GAME=crawl" "TILES="))))))))
    (synopsis "Roguelike dungeon crawler game")
    (description "Dungeon Crawl Stone Soup (also known as \"Crawl\" or DCSS
for short) is a roguelike adventure through dungeons filled with dangerous
monsters in a quest to find the mystifyingly fabulous Orb of Zot.")
    (home-page "https://crawl.develz.org")
    (license (list license:gpl2+
                   license:bsd-2
                   license:bsd-3
                   license:cc0
                   license:expat
                   license:zlib
                   license:asl2.0))))

(define-public crawl
  (deprecated-package "crawl" dungeon-crawl-stone-soup))

;; The linter here claims that patch file names should start with the package
;; name. But, in this case, the patches are inherited from crawl with the
;; "crawl-" prefix instead of "crawl-tiles-".
(define-public dungeon-crawl-stone-soup-tiles
  (package
    (inherit dungeon-crawl-stone-soup)
    (name "dungeon-crawl-stone-soup-tiles")
    (arguments
     (substitute-keyword-arguments
         (package-arguments crawl)
       ((#:make-flags flags)
        `(let ((dejavu (assoc-ref %build-inputs "font-dejavu")))
           (cons*
            (string-append "PROPORTIONAL_FONT=" dejavu
                           "/share/fonts/truetype/DejaVuSans.ttf")
            (string-append "MONOSPACED_FONT=" dejavu
                           "/share/fonts/truetype/DejaVuSansMono.ttf")
            "TILES=y"
            ;; Rename the executable to allow parallel installation with crawl.
            "GAME=crawl-tiles"
            ,flags)))))
    (inputs
     `(,@(package-inputs crawl)
       ("font-dejavu" ,font-dejavu)
       ("freetype6" ,freetype)
       ("glu" ,glu)
       ("libpng" ,libpng)
       ("sdl2" ,sdl2)
       ("sdl2-image" ,sdl2-image)
       ("sdl2-mixer" ,sdl2-mixer)))
    (native-inputs
     `(,@(package-native-inputs crawl)
       ("pngcrush" ,pngcrush)
       ("which" ,which)))
    (synopsis "Graphical roguelike dungeon crawler game")))

(define-public crawl-tiles
  (deprecated-package "crawl-tiles" dungeon-crawl-stone-soup-tiles))

(define-public lugaru
  (package
    (name "lugaru")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/osslugaru/lugaru/downloads/"
                                  "lugaru-" version ".tar.xz"))
              (sha256
               (base32
                "15zgcshy22q51rm72zi6y9z7qlgnz5iw3gczjdlir4bqmxy4gspk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DSYSTEM_INSTALL=ON")
       ;; no test target
       #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl2" ,sdl2)
       ("glu" ,glu)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("openal" ,openal)
       ("vorbis" ,libvorbis)
       ("zlib" ,zlib)))
    (home-page "https://osslugaru.gitlab.io")
    (synopsis "Cross-platform third-person action game")
    (description "Lugaru is a third-person action game.  The main character,
Turner, is an anthropomorphic rebel bunny rabbit with impressive combat skills.
In his quest to find those responsible for slaughtering his village, he uncovers
a far-reaching conspiracy involving the corrupt leaders of the rabbit republic
and the starving wolves from a nearby den.  Turner takes it upon himself to
fight against their plot and save his fellow rabbits from slavery.")
    (license (list license:gpl2+ ; code
                   ;; assets:
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0))))

(define-public 0ad-data
  (package
    (name "0ad-data")
    (version "0.0.23b-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.wildfiregames.com/0ad-"
                           version "-unix-data.tar.xz"))
       (file-name (string-append name "-" version ".tar.xz"))
       (sha256
        (base32
         "04x7729hk6zw1xj3n4s4lvaviijsnbjf5rhzvjxlr5fygvg4l6z1"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each (lambda (name)
                        (let* ((dir (string-append "binaries/data/mods/" name))
                               (file (string-append dir "/" name ".zip"))
                               (unzip #$(file-append unzip "/bin/unzip")))
                          (invoke unzip "-d" dir file)
                          (delete-file file)))
                      '("mod" "public"))
            #t))))
    (build-system trivial-build-system)
    (native-inputs `(("tar" ,tar)
                     ("xz" ,xz)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((out (assoc-ref %outputs "out"))
               (source (assoc-ref %build-inputs "source"))
               (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (xz-path (string-append (assoc-ref %build-inputs "xz") "/bin")))
           (setenv "PATH" xz-path)
           (mkdir out)
           (invoke tar "xvf" source "-C" out "--strip=3")))))
    (synopsis "Data files for 0ad")
    (description "0ad-data provides the data files required by the game 0ad.")
    (home-page "https://play0ad.com")
    (license (list (license:fsdg-compatible
                    "http://tavmjong.free.fr/FONTS/ArevCopyright.txt"
                    (license:license-comment
                     (package-license font-bitstream-vera)))
                   (package-license font-bitstream-vera)
                   license:cc-by-sa3.0
                   license:expat
                   license:gfl1.0
                   license:gpl2+
                   license:gpl3+))))

(define-public 0ad
  (package
    (name "0ad")
    (version "0.0.23b-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.wildfiregames.com/0ad-"
                           version "-unix-build.tar.xz"))
       (file-name (string-append name "-" version ".tar.xz"))
       (sha256
        (base32
         "0draa53xg69i5qhqym85658m45xhwkbiimaldj4sr3703rjgggq1"))))
       ;; A snippet here would cause a build failure because of timestamps
       ;; reset.  See https://bugs.gnu.org/26734.
    (inputs
     `(("0ad-data" ,0ad-data)
       ("curl" ,curl)
       ("enet" ,enet)
       ("gloox" ,gloox)
       ("icu4c" ,icu4c)
       ("libpng" ,libpng)
       ("libsodium" ,libsodium)
       ("libvorbis" ,libvorbis)
       ("libxcursor" ,libxcursor)
       ("libxml2" ,libxml2)
       ("miniupnpc" ,miniupnpc)
       ("mozjs-38" ,mozjs-38)
       ("openal" ,openal)
       ("sdl2" ,sdl2)
       ("wxwidgets" ,wxwidgets)
       ("zlib" ,zlib)))
    (native-inputs
     `(("boost" ,boost)
       ("cmake" ,cmake)
       ("mesa" ,mesa)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("config=release" "verbose=1" "-C" "build/workspaces/gcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-bundles
           (lambda _
             (delete-file-recursively "libraries/source/spidermonkey")
             #t))
         (add-after 'unpack 'fix-x11-includes
           (lambda _
             (substitute* "source/lib/sysdep/os/unix/x/x.cpp"
               (("<Xlib.h>") "<X11/Xlib.h>"))
             (substitute* "source/lib/sysdep/os/unix/x/x.cpp"
               (("<Xatom.h>") "<X11/Xatom.h>"))
             (substitute* "source/lib/sysdep/os/unix/x/x.cpp"
               (("<Xcursor/Xcursor.h>") "<X11/Xcursor/Xcursor.h>"))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((jobs (number->string (parallel-job-count)))
                    (out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (data (string-append out "/share/0ad")))
               (setenv "JOBS" (string-append "-j" jobs))
               (setenv "CC" "gcc")
               (with-directory-excursion "build/workspaces"
                 (invoke "./update-workspaces.sh"
                         (string-append "--libdir=" lib)
                         (string-append "--datadir=" data)
                         ;; TODO: "--with-system-nvtt"
                         "--with-system-mozjs38")))))
         (delete 'check)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (chdir "binaries")
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (data (string-append out "/share/0ad"))
                    (applications (string-append out "/share/applications"))
                    (pixmaps (string-append out "/share/pixmaps"))
                    (0ad-data (assoc-ref inputs "0ad-data")))
               ;; data
               (copy-recursively "data" data)
               (for-each (lambda (file)
                           (symlink (string-append 0ad-data "/" file)
                                    (string-append data "/" file)))
                         '("config" "mods/mod" "mods/public" "tools"))
               ;; libraries
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "system" "\\.so$"))
               ;; binaries
               (install-file "system/pyrogenesis" bin)
               (with-directory-excursion bin
                 (symlink "pyrogenesis" "0ad"))
               ;; resources
               (with-directory-excursion "../build/resources"
                 (install-file "0ad.desktop" applications)
                 (install-file "0ad.png" pixmaps))
               #t)))
         (add-after 'install 'check
           (lambda _
             (with-directory-excursion "system"
               (invoke "./test")))))))
    (home-page "https://play0ad.com")
    (synopsis "3D real-time strategy game of ancient warfare")
    (description "0 A.D. is a real-time strategy (RTS) game of ancient
warfare.  It's a historically-based war/economy game that allows players to
relive or rewrite the history of twelve ancient civilizations, each depicted
at their peak of economic growth and military prowess.

0ad needs a window manager that supports 'Extended Window Manager Hints'.")
    (license (list license:bsd-2
                   license:bsd-3
                   license:expat
                   license:gpl2+
                   license:ibmpl1.0
                   license:isc
                   license:lgpl2.1
                   license:lgpl3
                   license:mpl2.0
                   license:zlib))))

;; There have been no official releases.
(define-public open-adventure
  (let* ((commit "d43854f0f6bb8e9eea7fbce80348150e7e7fc34d")
         (revision "2"))
    (package
      (name "open-adventure")
      (version (string-append "2.5-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/esr/open-adventure")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "08bwrvf4axb1rsfd6ia1fddsky9pc1p350vjskhaakg2czc6dsk0"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "CC=gcc")
         #:parallel-build? #f ; not supported
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Linenoise is meant to be included, so we have to
               ;; copy it into the working directory.
               (let* ((linenoise (assoc-ref inputs "linenoise"))
                      (noisepath (string-append linenoise "/include/linenoise"))
                      (out (assoc-ref outputs "out")))
                 (copy-recursively noisepath "linenoise"))
               #t))
           (add-before 'build 'use-echo
             (lambda _
               (substitute* "tests/Makefile"
                 (("/bin/echo") (which "echo")))
               #t))
           (add-after 'build 'build-manpage
             (lambda _
               ;; This target is missing a dependency
               (substitute* "Makefile"
                 ((".adoc.6:" line)
                  (string-append line " advent.adoc")))
               (invoke "make" ".adoc.6")))
           ;; There is no install target
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (man (string-append out "/share/man/man6")))
                 (install-file "advent" bin)
                 (install-file "advent.6" man))
               #t)))))
      (native-inputs
       `(("asciidoc" ,asciidoc)
         ("linenoise" ,linenoise)
         ("python" ,python)
         ("python-pyyaml" ,python-pyyaml)))
      (home-page "https://gitlab.com/esr/open-adventure")
      (synopsis "Colossal Cave Adventure")
      (description "The original Colossal Cave Adventure from 1976 was the
origin of all text adventures, dungeon-crawl (computer) games, and
computer-hosted roleplaying games.  This is the last version released by
Crowther & Woods, its original authors, in 1995.  It has been known as
\"adventure 2.5\" and \"430-point adventure\".")
      (license license:bsd-2))))

(define-public tales-of-maj-eyal
  (package
    (name "tales-of-maj-eyal")
    (version "1.5.10")
    (synopsis "Single-player, RPG roguelike game set in the world of Eyal")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://te4.org/dl/t-engine/t-engine4-src-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0mc5dgh2x9nbili7gy6srjhb23ckalf08wqq2amyjr5rq392jvd7"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* '("src/music.h" "src/tSDL.h")
             (("#elif defined(__FreeBSD__)" line)
              (string-append
               line " || defined(__GNUC__)")))
           (substitute* '("src/tgl.h")
             (("#include <GL/glext.h>") ""))
           #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))
       ("glu" ,glu)
       ("premake4" ,premake4)
       ("openal" ,openal)
       ("vorbis" ,libvorbis)
       ("luajit" ,luajit)))
    (arguments
     `(#:make-flags '("CC=gcc" "config=release")
       #:phases (modify-phases %standard-phases
                  (delete 'bootstrap)
                  (replace 'configure
                    (lambda _
                      (invoke "premake4" "gmake")
                      #t))
                  (add-after 'set-paths 'set-sdl-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "CPATH"
                              (string-append (assoc-ref inputs "sdl-union")
                                             "/include/SDL2"))
                      #t))
                  (delete 'check)
                  ;; premake doesn't provide install target
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (usr (string-append out "/usr"))
                             (bin (string-append out "/bin"))
                             (licenses (string-append out "/share/licenses"))
                             (documents (string-append out "/share/doc"))
                             (pixmaps (string-append out "/share/pixmaps"))
                             (icon "te4-icon.png")
                             (data (string-append out "/share/" ,name))
                             (applications (string-append
                                            out "/share/applications"))
                             (unzip (string-append
                                     (assoc-ref inputs "unzip") "/bin/unzip"))
                             (wrapper (string-append bin "/" ,name)))
                        ;; icon
                        (mkdir-p pixmaps)
                        (system* unzip "-j"
                                 (string-append
                                  "game/engines/te4-" ,version ".teae")
                                 (string-append
                                  "data/gfx/" icon) "-d" pixmaps)
                        ;; game executable
                        (install-file "t-engine" data)
                        (mkdir-p bin)
                        (with-output-to-file wrapper
                          (lambda ()
                            (display
                             (string-append
                              "#!/bin/sh\n"
                              ;; No bootstrap code found,
                              ;; defaulting to working directory
                              ;; for engine code!
                              "cd " data "\n"
                              "exec -a tome4 ./t-engine \"$@\"\n"))))
                        (chmod wrapper #o555)
                        ;; licenses
                        (for-each (lambda (file)
                                    (install-file file licenses))
                                  '("COPYING" "COPYING-MEDIA"))
                        ;; documents
                        (for-each (lambda (file)
                                    (install-file file documents))
                                  '("CONTRIBUTING" "CREDITS"))
                        ;; data
                        (copy-recursively "bootstrap" (string-append
                                                       data "/bootstrap"))
                        (copy-recursively "game" (string-append data "/game"))
                        ;; launcher
                        (mkdir-p applications)
                        (with-output-to-file (string-append applications "/"
                                                            ,name ".desktop")
                          (lambda ()
                            (display
                             (string-append
                              "[Desktop Entry]
Name=ToME4
Comment=" ,synopsis "\n"
"Exec=" ,name "\n"
"Icon=" icon "\n"
"Terminal=false
Type=Application
Categories=Game;RolePlaying;\n")))))
                      #t)))))
    (home-page "https://te4.org")
    (description "Tales of Maj’Eyal (ToME) RPG, featuring tactical turn-based
combat and advanced character building.  Play as one of many unique races and
classes in the lore-filled world of Eyal, exploring random dungeons, facing
challenging battles, and developing characters with your own tailored mix of
abilities and powers.  With a modern graphical and customisable interface,
intuitive mouse control, streamlined mechanics and deep, challenging combat,
Tales of Maj’Eyal offers engaging roguelike gameplay for the 21st century.")
    (license license:gpl3+)))

(define-public tome4
  (deprecated-package "tome4" tales-of-maj-eyal))

(define-public quakespasm
  (package
    (name "quakespasm")
    (version "0.93.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/quakespasm/Source/quakespasm-"
                           version ".tgz"))
       (sha256
        (base32
         "1bimv18f6rzhyjz78yvw2vqr5n0kdqbcqmq7cb3m951xgsxfcgpd"))))
    (arguments
     `(#:tests? #f
       #:make-flags '("CC=gcc"
                      "MP3LIB=mpg123"
                      "USE_CODEC_FLAC=1"
                      "USE_CODEC_MIKMOD=1"
                      "USE_SDL2=1"
                      "-CQuake")
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'fix-makefile-paths
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        (substitute* "Quake/Makefile"
                          (("/usr/local/games")
                           (string-append out "/bin")))
                        #t))))))
    (build-system gnu-build-system)
    (inputs `(("libmikmod" ,libmikmod)
              ("libvorbis" ,libvorbis)
              ("flac" ,flac)
              ("mesa" ,mesa)
              ("mpg123" ,mpg123)
              ("sdl2" ,sdl2)))
    (synopsis "First person shooter engine for Quake 1")
    (description "Quakespasm is a modern engine for id software's Quake 1.
It includes support for 64 bit CPUs, custom music playback, a new sound driver,
some graphical niceities, and numerous bug-fixes and other improvements.")
    (home-page "http://quakespasm.sourceforge.net/")
    (license license:gpl2+)))

(define-public vkquake
  (package
    (inherit quakespasm)
    (name "vkquake")
    (version "1.01.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Novum/vkQuake.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iwin8j5kbyrknbkhjgpy8nmm7pxqzr0daa9gn7p38qhg2mh0a39"))))
    (arguments
     `(#:make-flags
       (let ((vulkanlib (string-append (assoc-ref %build-inputs
                                                  "vulkan-loader") "/lib")))
         (list "CC=gcc"
               "MP3LIB=mpg123"
               "USE_CODEC_FLAC=1"
               "USE_CODEC_MIKMOD=1"
               "USE_SDL2=1"
               (string-append "LDFLAGS=-Wl,-rpath=" vulkanlib)
               "-CQuake"))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'fix-makefile-paths
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((vulkan (assoc-ref %build-inputs
                                               "vulkan-loader"))
                            (out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        (substitute* "Quake/Makefile" ((" /usr")
                                                       (string-append " " out)))
                        (substitute* "Quake/Makefile" (("/games")
                                                       (string-append "/bin")))
                        (substitute* "Quake/Makefile" (("..VULKAN_SDK.") vulkan))
                        #t))))
       ,@(strip-keyword-arguments '(#:make-flags #:phases)
                                  (package-arguments quakespasm))))
    (inputs `(("vulkan-headers" ,vulkan-headers)
              ("vulkan-loader" ,vulkan-loader)
              ,@(package-inputs quakespasm)))
    (description "vkquake is a modern engine for id software's Quake 1.
It includes support for 64 bit CPUs, custom music playback, a new sound driver,
some graphical niceities, and numerous bug-fixes and other improvements.")
    (home-page "https://github.com/Novum/vkQuake")))

(define-public yamagi-quake2
  (package
    (name "yamagi-quake2")
    (version "7.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://deponie.yamagi.org/quake2/quake2-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0psinbg25mysd58k99s1n34w31w5hj1vppb39gdjb0zqi6sl6cps"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list "CC=gcc"
             ;; link openAL instead of using dlopen at runtime
             "DLOPEN_OPENAL=\"no\""
             ;; an optional directory where it will look for quake2 data files
             ;; in addition to the current working directory
             "WITH_SYSTEMWIDE=yes"
             "WITH_SYSTEMDIR=\"/opt/quake2\"")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/bin"))
               ;; The yamagi-quake2 binary must be in the same directory
               ;; as it's engine libraries, but symlinking it to /bin is okay
               ;; https://github.com/yquake2/yquake2/blob/master/stuff/packaging.md
               (copy-recursively "release"
                                 (string-append out "/lib/yamagi-quake2"))
               (symlink (string-append out "/lib/yamagi-quake2/quake2")
                        (string-append out "/bin/yamagi-quake2"))
               (symlink (string-append out "/lib/yamagi-quake2/q2ded")
                        (string-append out "/bin/yamagi-q2ded"))))))))
    (inputs `(("sdl2" ,sdl2)
              ("mesa" ,mesa)
              ("libvorbis" ,libvorbis)
              ("zlib" ,zlib)
              ("openal" ,openal)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "First person shooter engine based on quake2")
    (description "Yamagi Quake II is an enhanced client for id Software's Quake II.
The main focus is an unchanged single player experience like back in 1997,
thus the gameplay and the graphics are unaltered.  However the user may use one
of the unofficial retexturing packs.  In comparison with the official client,
over 1000 bugs were fixed and an extensive code audit done,
making Yamagi Quake II one of the most solid Quake II implementations available.")
    (home-page "https://www.yamagi.org/quake2/")
    (license (list license:gpl2+         ; game and server
                   (license:non-copyleft ; info-zip
                    "file://LICENSE"
                    "See Info-Zip section.")
                   license:public-domain)))) ; stb

(define-public nudoku
  (package
    (name "nudoku")
    (version "1.0.0")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://github.com/jubalh/nudoku/"
                                  "releases/download/" version
                                  "/nudoku-" version ".tar.xz"))
	      (sha256
               (base32
                "0nr2j2z07nxk70s8xnmmpzccxicf7kn5mbwby2kg6aq8paarjm8k"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://jubalh.github.io/nudoku/")
    (synopsis "Sudoku for your terminal")
    (description "Nudoku is a ncurses-based Sudoku game for your terminal.")
    (license license:gpl3+)))

(define-public the-butterfly-effect
  (package
    (name "the-butterfly-effect")
    (version "0.9.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/the-butterfly-effect/tbe/archive/"
             "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18qkp7fgdvyl3haqqa693mgyic7afsznsxgz98z9wn4csaqxsnby"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; There is no "install" phase.  By default, tbe is installed
         ;; in the build directory.  Provide our own installation.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share")))
               (install-file "usr/games/tbe" bin)
               (mkdir-p share)
               (copy-recursively "usr/share" share)
               #t))))
       ;; Test suite requires a running Xorg server. Even when
       ;; provided, it fails with "D-Bus library appears to be
       ;; incorrectly set up; failed to read machine uuid: Failed to
       ;; open "/etc/machine-id": No such file or directory" along
       ;; with multiple "QPainter:: ... Painter not active" warnings.
       #:tests? #f))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (native-inputs
     `(("cmake" ,cmake)
       ("gettext-minimal" ,gettext-minimal)
       ("qttools" ,qttools)))
    (synopsis "Realistic physics puzzle game")
    (description "The Butterfly Effect (tbe) is a game that uses
realistic physics simulations to combine lots of simple mechanical
elements to achieve a simple goal in the most complex way possible.")
    (home-page "http://the-butterfly-effect.org/")
    ;; Main license is GPL2-only.  However, artwork is distributed
    ;; under various licenses, listed here.
    (license (list license:gpl2 license:public-domain license:expat
                   license:cc-by-sa3.0 license:gpl3+ license:wtfpl2))))

(define-public pioneer
  (package
    (name "pioneer")
    (version "20180203")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pioneerspacesim/pioneer.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hp2mf36kj2v93hka8m8lxw2qhmnjc62wjlpw7c7ix0r8xa01i6h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("assimp" ,assimp)
       ("curl" ,curl)
       ("freetype" ,freetype)
       ("glu" ,glu)
       ("libpng" ,libpng)
       ("libsigc++" ,libsigc++)
       ("libvorbis" ,libvorbis)
       ("lua" ,lua-5.2)                 ;not compatible with 5.3
       ("mesa" ,mesa)
       ("sdl" ,(sdl-union (list sdl2 sdl2-image)))))
    (arguments
     `(#:tests? #f                      ;tests are broken
       #:configure-flags (list "--with-external-liblua"
                               (string-append "PIONEER_DATA_DIR="
                                              %output "/share/games/pioneer"))
       #:phases (modify-phases %standard-phases
                  (add-before 'bootstrap 'fix-lua-check
                    (lambda _
                      (substitute* "configure.ac"
                        (("lua5.2")
                         (string-append "lua-" ,(version-major+minor
                                                 (package-version lua-5.2))))))))))
    (home-page "http://pioneerspacesim.net")
    (synopsis "Game of lonely space adventure")
    (description
     "Pioneer is a space adventure game set in our galaxy at the turn of the
31st century.  The game is open-ended, and you are free to eke out whatever
kind of space-faring existence you can think of.  Look for fame or fortune by
exploring the millions of star systems.  Turn to a life of crime as a pirate,
smuggler or bounty hunter.  Forge and break alliances with the various
factions fighting for power, freedom or self-determination.  The universe is
whatever you make of it.")
    (license license:gpl3)))

(define-public badass
  (let ((commit "3c3cd669b4fc8f73a102e3702788f7b28dc47dbb")
        (revision "0"))
  (package
    (name "badass")
    (version (git-version "0.0" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/umayr/badass.git")
                     (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05c9vdcb5ym3z0n5ll3v39mw4yl9jcjnlydmn0yl89ai9pv71zb6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/umayr/badass"))
    (synopsis "Hacking contribution graphs in git")
    (description
     "Badass generates false commits for a range of dates, essentially
hacking the gamification of contribution graphs on platforms such as
Github or Gitlab.")
    (home-page "https://github.com/umayr/badass")
    (license license:expat))))

(define-public colobot
  (package
    (name "colobot")
    (version "0.1.12-alpha")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/colobot/colobot.git")
             (commit (string-append "colobot-gold-" version))
             (recursive? #t)))          ;for "data/" subdir
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1c181cclkrnspgs07lvndg2c81cjq3smkv7qim8c470cj88rcrp2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-after 'unpack 'fix-directories
           (lambda _
             (substitute* "CMakeLists.txt"
               (("(\\$\\{CMAKE_INSTALL_PREFIX\\})/games" _ prefix)
                (string-append prefix "/bin"))
               (("(\\$\\{CMAKE_INSTALL_PREFIX\\}/share)/games/colobot" _ prefix)
                (string-append prefix "/colobot")))
             #t))
         (add-after 'fix-directories 'install-music
           ;; Retrieve and install music files.
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Installation process tries to download music files using
             ;; "wget" if not already present.  Since we are going another
             ;; route, skip "wget" command check.
             (substitute* "data/music/CMakeLists.txt"
               (("find_program\\(WGET wget\\)") ""))
             ;; Populate "music/" directory.
             (let ((data (assoc-ref inputs "colobot-music")))
               (invoke "tar" "-xvf" data "-Cdata/music"))
             #t)))))
    (native-inputs
     `(("colobot-music"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://colobot.info/files/music/"
                               "colobot-music_ogg_" version ".tar.gz"))
           (sha256
            (base32
             "1s86cd36rwkff329mb1ay1wi5qqyi35564ppgr3f4qqz9wj9vs2m"))))
       ("gettext" ,gettext-minimal)
       ("librsvg" ,librsvg)
       ("po4a" ,po4a)
       ("python" ,python-wrapper)))
    (inputs
     `(("boost" ,boost)
       ("glew" ,glew)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libsndfile" ,libsndfile)
       ("libvorbis" ,libvorbis)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("sdl" ,(sdl-union (list sdl2 sdl2-image sdl2-ttf)))))
    (synopsis "Educational programming strategy game")
    (description "Colobot: Gold Edition is a real-time strategy game, where
you can program your units (bots) in a language called CBOT, which is similar
to C++ and Java.  Your mission is to find a new planet to live and survive.
You can save humanity and get programming skills!")
    (home-page "https://colobot.info")
    (license license:gpl3+)))

(define-public gzdoom
  (package
    (name "gzdoom")
    (version "3.7.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://zdoom.org/files/gzdoom/src/gzdoom-src-g"
                              version ".zip"))
              (sha256
               (base32
                "0182f160m8d0c3nywjw3dxvnz93xjs4cn8akx7137cha4s05wdq7"))
              (patches (search-patches "gzdoom-search-in-installed-share.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "bzip2")
                  (delete-file-recursively "game-music-emu")
                  (delete-file-recursively "jpeg")
                  (delete-file-recursively "zlib")
                  #t))))
    (arguments
     '(#:tests? #f
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list
          (string-append
           "-DCMAKE_CXX_FLAGS:="
           "-DSHARE_DIR=\\\"" out "/share/\\\" "
           "-DGUIX_OUT_PK3=\\\"" out "/share/games/doom\\\"")
          ;; look for libraries at buildtime instead of
          ;; dynamically finding them at runtime
          "-DDYN_OPENAL=OFF"
          "-DDYN_FLUIDSYNTH=OFF"
          "-DDYN_GTK=OFF"
          "-DDYN_MPG123=OFF"
          "-DDYN_SNDFILE=OFF"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-referenced-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((fluid-3 (assoc-ref inputs "fluid-3"))
                   (timidity++ (assoc-ref inputs "timidity++"))
                   (out (assoc-ref outputs "out")))

               (substitute*
                   "src/CMakeLists.txt"
                 (("COMMAND /bin/sh")
                  (string-append "COMMAND " (which "sh"))))

               (substitute*
                   "src/sound/mididevices/music_fluidsynth_mididevice.cpp"
                 (("/usr/share/sounds/sf2/FluidR3_GM.sf2")
                  (string-append fluid-3 "/share/soundfonts/FluidR3Mono_GM.sf3")))

               (substitute*
                   "src/sound/mididevices/music_timiditypp_mididevice.cpp"
                 (("exename = \"timidity\"")
                  (string-append "exename = \"" timidity++ "/bin/timidity\"")))
               #t))))))
    (build-system cmake-build-system)
    (inputs `(("bzip2" ,bzip2)
              ("fluid-3" ,fluid-3)
              ("fluidsynth" ,fluidsynth-1)      ;XXX: try using 2.x when updating
              ("gtk+3" ,gtk+)
              ("libgme" ,libgme)
              ("libjpeg" ,libjpeg)
              ("libsndfile" ,libsndfile)
              ("mesa" ,mesa)
              ("mpg123" ,mpg123)
              ("openal" ,openal)
              ("sdl2" ,sdl2)
              ("timidity++" ,timidity++)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("unzip" ,unzip)))
    (synopsis "Modern Doom 2 source port")
    (description "GZdoom is a port of the Doom 2 game engine, with a modern
renderer.  It improves modding support with ZDoom's advanced mapping features
and the new ZScript language.  In addition to Doom, it supports Heretic, Hexen,
Strife, Chex Quest, and fan-created games like Harmony, Hacx and Freedoom.")
    (home-page "https://zdoom.org/index")
    ;; The source uses x86 assembly
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license (list license:gpl3+         ; gzdoom game
                   license:lgpl3+        ; gzdoom renderer
                   license:expat         ; gdtoa
                   (license:non-copyleft ; modified dumb
                    "file://dumb/licence.txt"
                    "Dumb license, explicitly GPL compatible.")))))

(define-public odamex
  (package
    (name "odamex")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/odamex/Odamex/" version "/"
             "odamex-src-" version ".tar.gz"))
       (sha256
        (base32
         "1sh6lqj7vsdmnqz17hw0b6vy7xx6dp41k2sdw99ympsfa2xd1d2j"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no tests.
    (inputs
     `(("sdl" ,sdl)
       ("sdl-mixer" ,sdl-mixer)
       ("zlib" ,zlib)
       ("libpng" ,libpng)
       ("alsa-lib" ,alsa-lib)))
    (home-page "https://odamex.net/")
    (synopsis "Multiplayer Doom port")
    (description "Odamex is a modification of the Doom engine that
allows players to easily join servers dedicated to playing Doom
online.")
    (license license:gpl2+)))

(define-public fortune-mod
  (package
    (name "fortune-mod")
    (version "2.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shlomif/fortune-mod")
             (commit (string-append "fortune-mod-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11xff87s8ifw2dqs90n0rjq0psv4i7ykybygmibsqjj7id3xxw4c"))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "check"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-env
           (lambda* (#:key inputs #:allow-other-keys)
             (use-modules (guix build utils))
             (let* ((cmake-rules (assoc-ref inputs "cmake-rules")))
               (copy-file cmake-rules
                          (string-append "fortune-mod/cmake/"
                                         (strip-store-file-name cmake-rules)))
               (chdir "fortune-mod")
               ;; TODO: Valgrind tests fail for some reason.
               ;; Similar issue: https://github.com/shlomif/fortune-mod/issues/21 (?)
               (delete-file "tests/t/valgrind.t")
               #t)))
         (add-after 'install 'fix-install-directory
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move binary from "games/" to "bin/" and remove the latter.  This
             ;; is easier than patching CMakeLists.txt since the tests hard-code
             ;; the location as well.
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (games (string-append out "/games")))
               (rename-file (string-append games "/fortune")
                            (string-append bin "/fortune"))
               (rmdir games)
               #t))))))
    (inputs `(("recode" ,recode)))
    (native-inputs
     `(("perl" ,perl)
       ;; The following is only needed for tests.
       ("perl-file-find-object" ,perl-file-find-object)
       ("perl-test-differences" ,perl-test-differences)
       ("perl-class-xsaccessor" ,perl-class-xsaccessor)
       ("perl-io-all" ,perl-io-all)
       ("perl-test-runvalgrind" ,perl-test-runvalgrind)
       ("cmake-rules"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://bitbucket.org/shlomif/shlomif-cmake-modules/"
                               "raw/c505713d7a7cda608f97f01577e5868a711b883e/"
                               "shlomif-cmake-modules/Shlomif_Common.cmake"))
           (sha256
            (base32 "0kx9s1qqhhzprp1w3b67xmsns0n0v506bg5hgrshxaxpy6lqiwb2"))))))
    (home-page "http://www.shlomifish.org/open-source/projects/fortune-mod/")
    (synopsis "The Fortune Cookie program from BSD games")
    (description "Fortune is a command-line utility which displays a random
quotation from a collection of quotes.")
    (license license:bsd-4)))

(define xonotic-data
  (package
    (name "xonotic-data")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dl.xonotic.org/xonotic-"
                           version ".zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "1mcs6l4clvn7ibfq3q69k2p0z6ww75rxvnngamdq5ic6yhq74bx2"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (xonotic (string-append out "/share/xonotic"))
                (source (assoc-ref %build-inputs "source"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (copy-file source (string-append ,name "-" ,version ".zip"))
           (invoke unzip (string-append ,name "-" ,version ".zip"))
           (mkdir-p out)
           (mkdir-p xonotic)
           (chdir "Xonotic")
           (copy-recursively "data"
                             (string-append xonotic "/data"))
           (copy-recursively "server"
                             (string-append xonotic "/server"))
           (install-file "key_0.d0pk" xonotic)))))
    (home-page "http://xonotic.org")
    (synopsis "Data files for Xonotic")
    (description
     "Xonotic-data provides the data files required by the game Xonotic.")
    (license (list license:gpl2+
                   (license:x11-style "file://server/rcon.pl")))))

(define-public xonotic
  (package
    (name "xonotic")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dl.xonotic.org/xonotic-"
                           version "-source.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "0axxw04fyz6jlfqd0kp7hdrqa0li31sx1pbipf2j5qp9wvqicsay"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--prefix="
                                              (assoc-ref %outputs "out"))
                               "--disable-rijndael")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'make-darkplaces
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sharedir (string-append out "/share/xonotic/")))
               (invoke "make" "-C" "source/darkplaces"
                       (string-append "DP_FS_BASEDIR="
                                      sharedir)
                       "DP_LINK_TO_LIBJPEG=1"
                       "DP_SOUND_API=ALSA"
                       "CC=gcc"
                       "-f" "makefile"
                       "cl-release")
               (invoke "make" "-C" "source/darkplaces"
                       (string-append "DP_FS_BASEDIR="
                                      sharedir)
                       "DP_LINK_TO_LIBJPEG=1"
                       "DP_SOUND_API=ALSA"
                       "CC=gcc"
                       "-f" "makefile"
                       "sdl-release")
               (invoke "make" "-C" "source/darkplaces"
                       (string-append "DP_FS_BASEDIR="
                                      sharedir)
                       "DP_LINK_TO_LIBJPEG=1"
                       "DP_SOUND_API=ALSA"
                       "CC=gcc"
                       "-f" "makefile"
                       "sv-release"))))
         (add-before 'configure 'bootstrap
           (lambda _
             (chdir "source/d0_blind_id")
             (invoke "sh" "autogen.sh")))
         (add-after 'build 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Add .desktop files for the 2 variants and the symlink
             (let* ((output (assoc-ref outputs "out"))
                    (apps (string-append output "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file
                   (string-append apps "/xonotic-glx.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                     Name=xonotic-glx~@
                     Comment=Xonotic glx~@
                     Exec=~a/bin/xonotic-glx~@
                     TryExec=~@*~a/bin/xonotic-glx~@
                     Icon=xonotic~@
                     Categories=Game~@
                     Type=Application~%"
                           output)))
               (with-output-to-file
                   (string-append apps "/xonotic-sdl.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                     Name=xonotic-sdl~@
                     Comment=Xonotic sdl~@
                     Exec=~a/bin/xonotic-sdl~@
                     TryExec=~@*~a/bin/xonotic-sdl~@
                     Icon=xonotic~@
                     Categories=Game~@
                     Type=Application~%"
                           output)))
               (with-output-to-file
                   (string-append apps "/xonotic.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                     Name=xonotic~@
                     Comment=Xonotic~@
                     Exec=~a/bin/xonotic-glx~@
                     TryExec=~@*~a/bin/xonotic~@
                     Icon=xonotic~@
                     Categories=Game~@
                     Type=Application~%"
                           output)))
               #t)))
         (add-after 'install-desktop-entry 'install-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "../../misc/logos/icons_png/"
                 (for-each
                  (lambda (file)
                    (let* ((size (string-filter char-numeric? file))
                           (icons (string-append out "/share/icons/hicolor/"
                                                 size "x" size "/apps")))
                      (mkdir-p icons)
                      (copy-file file (string-append icons "/xonotic.png"))))
                  '("xonotic_16.png" "xonotic_22.png" "xonotic_24.png"
                    "xonotic_32.png" "xonotic_48.png" "xonotic_64.png"
                    "xonotic_128.png" "xonotic_256.png" "xonotic_512.png"))))))
         (add-after 'install-icons 'install-binaries
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (define (install src dst)
                 (let ((dst (string-append out dst)))
                   (mkdir-p (dirname dst))
                   (copy-file src dst)))
               (mkdir-p (string-append out "/bin"))
               (install "../darkplaces/darkplaces-dedicated"
                        "/bin/xonotic-dedicated")
               (install "../darkplaces/darkplaces-glx"
                        "/bin/xonotic-glx")
               (install "../darkplaces/darkplaces-sdl"
                        "/bin/xonotic-sdl")
               ;; Provide a default xonotic executable, defaulting to SDL.
               (symlink (string-append out "/bin/xonotic-sdl")
                        (string-append out "/bin/xonotic"))
               #t)))
         (add-after 'install-binaries 'install-data
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (data (assoc-ref inputs "xonotic-data")))
               (symlink (string-append data "/share/xonotic")
                        (string-append out "/share/xonotic"))
               #t)))
         (add-after 'install-binaries 'wrap-binaries
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Curl and libvorbis need to be wrapped so that we get
             ;; sound and networking.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/xonotic"))
                    (bin-sdl (string-append out "/bin/xonotic-sdl"))
                    (bin-glx (string-append out "/bin/xonotic-glx"))
                    (bin-dedicated (string-append out "/bin/xonotic-dedicated"))
                    (curl (assoc-ref inputs "curl"))
                    (vorbis (assoc-ref inputs "libvorbis")))
               (wrap-program bin
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append curl "/lib:" vorbis "/lib"))))
               (wrap-program bin-sdl
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append curl "/lib:" vorbis "/lib"))))
               (wrap-program bin-glx
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append curl "/lib:" vorbis "/lib"))))
               (wrap-program bin-dedicated
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append curl "/lib:" vorbis "/lib"))))
               #t))))))
    (inputs
     `(("xonotic-data" ,xonotic-data)
       ("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("libjpeg" ,libjpeg)
       ("libmodplug" ,libmodplug)
       ("libvorbis" ,libvorbis)
       ("libogg" ,libogg)
       ("libxpm" ,libxpm)
       ("libxxf86dga" ,libxxf86dga)
       ("libxxf86vm" ,libxxf86vm)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxau" ,libxau)
       ("libxdmcp" ,libxdmcp)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("freetype" ,freetype)
       ("sdl2" ,sdl2)
       ("libpng" ,libpng)
       ("hicolor-icon-theme" ,hicolor-icon-theme)))
    (native-inputs
     `(("unzip" ,unzip)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ("gmp" ,gmp)))
    (home-page "http://xonotic.org")
    (synopsis "Fast-paced first-person shooter game")
    (description
     "Xonotic is a free, fast-paced first-person shooter.
The project is geared towards providing addictive arena shooter
gameplay which is all spawned and driven by the community itself.
Xonotic is a direct successor of the Nexuiz project with years of
development between them, and it aims to become the best possible
open-source FPS of its kind.")
    (license (list license:gpl2+
                   license:bsd-3 ; /source/d0_blind_id folder and others
                   (license:x11-style "" "See file rcon.pl.")))))

(define-public frotz
  (package
    (name "frotz")
    (version "2.44")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "http://www.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")
                         (string-append
                          "ftp://ftp.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")))
              (sha256
               (base32
                "1v735xr3blznac8fnwa27s1vhllx4jpz7kw7qdw1bsfj6kq21v3k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'curses
           (lambda _
             (substitute* "Makefile"
               (("lcurses") "lncurses"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man6")))
               (install-file "frotz" bin)
               (mkdir-p man)
               (install-file "doc/frotz.6" man)
               #t))))))
    (inputs `(("libmodplug" ,libmodplug)
              ("libsamplerate" ,libsamplerate)
              ("libsndfile" ,libsndfile)
              ("libvorbis" ,libvorbis)
              ("ncurses" ,ncurses)))
    (synopsis "Portable Z-machine interpreter (ncurses version) for text adventure games")
    (description "Frotz is an interpreter for Infocom games and other Z-machine
games in the text adventure/interactive fiction genre.  This version of Frotz
complies with standard 1.0 of Graham Nelson's specification.  It plays all
Z-code games V1-V8, including V6, with sound support through libao, and uses
ncurses for text display.")
    (home-page "http://frotz.sourceforge.net")
    (license license:gpl2+)))

(define-public frotz-dumb-terminal
  (package
    (name "frotz-dumb-terminal")
    (version "2.44")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "http://www.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")
                         (string-append
                          "ftp://ftp.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")))
              (sha256
               (base32
                "1v735xr3blznac8fnwa27s1vhllx4jpz7kw7qdw1bsfj6kq21v3k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make" "dumb")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man6")))
               (install-file "dfrotz" bin)
               (mkdir-p man)
               (install-file "doc/dfrotz.6" man)
               #t))))))
    (synopsis "Portable Z-machine dumb interpreter for text adventure games")
    (description "Frotz is an interpreter for Infocom games and
other Z-machine games in the text adventure/interactive fiction genre.
dfrotz is the dumb interface version.  You get no screen control; everything
is just printed to the terminal line by line.  The terminal handles all the
scrolling.  Maybe you'd like to experience what it's like to play Adventure on
a teletype.  A much cooler use for compiling Frotz with the dumb interface is
that it can be wrapped in CGI scripting, PHP, and the like to allow people
to play games on webpages.  It can also be made into a chat bot.")
    (home-page "http://frotz.sourceforge.net")
    (license license:gpl2+)))

(define-public frotz-sdl
  (let* ((commit "4de8c34f2116fff554af6216c30ec9d41bf50b24"))
    (package
      (name "frotz-sdl")
      (version "2.45pre")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/DavidGriffith/frotz")
                      (commit commit)))
                (sha256
                 (base32
                  "18ms21pcrl7ipcnyqnf8janamkryzx78frsgd9kfk67jvbj0z2k8"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'patch-makefile
             (lambda _
               (substitute* "Makefile"
                 (("lcurses") "lncurses")
                 (("^BUILD_DATE_TIME =.*$")
                  "BUILD_DATE_TIME = \"2.45pre-20180907.00000\"\n"))
               #t))
           (replace 'build
             (lambda _
               (invoke "make" "sdl")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (man (string-append out "/share/man/man6")))
                 (install-file "sfrotz" bin)
                 (mkdir-p man)
                 (install-file "doc/sfrotz.6" man)
                 #t))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("which" ,which)
         ("perl" ,perl)))
      (inputs `(("sdl2" ,sdl2)
                ("sdl2-mixer" ,sdl2-mixer)
                ("libmodplug" ,libmodplug)
                ("libsamplerate" ,libsamplerate)
                ("libsndfile" ,libsndfile)
                ("libvorbis" ,libvorbis)
                ("ncurses" ,ncurses)
                ("freetype" ,freetype)
                ("libjpeg-turbo" ,libjpeg-turbo)))
      (synopsis "Portable Z-machine interpreter (SDL port) for text adventure games")
      (description "Frotz is an interpreter for Infocom games and other Z-machine
games in the text adventure/interactive fiction genre.  This version of Frotz
using SDL fully supports all these versions of the Z-Machine including the
graphical version 6.  Graphics and sound are created through the use of the SDL
libraries.  AIFF sound effects and music in MOD and OGG formats are supported
when packaged in Blorb container files or optionally from individual files.")
      (home-page "http://frotz.sourceforge.net")
      (license license:gpl2+))))

(define-public libmanette
  (package
    (name "libmanette")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libmanette/"
                                  (version-major+minor version) "/"
                                  "libmanette-" version ".tar.xz"))
              (sha256
               (base32
                "1lpprk2qz1lsqf9xj6kj2ciyc1zmjhj5lwd584qkh7jgz2x9y6wb"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-compile-resources
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("libevdev" ,libevdev)
       ("libgudev" ,libgudev)))
    (home-page "https://wiki.gnome.org/Apps/Games")
    (synopsis "Game controller library")
    (description "Libmanette is a small GObject library giving you simple
access to game controllers.  It supports the de-facto standard gamepads as
defined by the W3C standard Gamepad specification or as implemented by the SDL
GameController.")
    (license license:lgpl2.1+)))

(define-public quadrapassel
  (package
    (name "quadrapassel")
    (version "3.31.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/quadrapassel/"
                                  (version-major+minor version) "/"
                                  "quadrapassel-" version ".tar.xz"))
              (sha256
               (base32
                "08i01nsgfb502xzzrrcxxbs7awb0j1h4c08vmj0j18ipa1sz8vb8"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ;for desktop-file-validate
       ("gettext" ,gnu-gettext)
       ("glib" ,glib "bin")             ;for glib-compile-resources
       ("itstool" ,itstool)
       ("libxml2" ,libxml2)             ;for xmllint
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libmanette" ,libmanette)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Quadrapassel")
    (synopsis "GNOME version of Tetris")
    (description "Quadrapassel comes from the classic falling-block game,
Tetris.  The goal of the game is to create complete horizontal lines of
blocks, which will disappear.  The blocks come in seven different shapes made
from four blocks each: one straight, two L-shaped, one square, and two
S-shaped.  The blocks fall from the top center of the screen in a random
order.  You rotate the blocks and move them across the screen to drop them in
complete lines.  You score by dropping blocks fast and completing lines.  As
your score gets higher, you level up and the blocks fall faster.")
    (license license:gpl2+)))

(define-public endless-sky
  (package
    (name "endless-sky")
    (version "0.9.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/endless-sky/endless-sky")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0i36lawypikbq8vvzfis1dn7yf6q0d2s1cllshfn7kmjb6pqfi6c"))))
    (build-system scons-build-system)
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-resource-locations
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "source/Files.cpp"
               (("/usr/local/")
                (string-append (assoc-ref outputs "out") "/")))
             #t))
         (add-after 'unpack 'patch-scons
           (lambda _
             (substitute* "SConstruct"
               ;; Keep environmental variables
               (("Environment\\(\\)")
                "Environment(ENV = os.environ)")
               ;; Install into %out/bin
               (("games\"") "bin\""))
             #t)))))
    (inputs
     `(("glew" ,glew)
       ("libjpeg" ,libjpeg-turbo)
       ("libmad" ,libmad)
       ("libpng" ,libpng)
       ("openal" ,openal)
       ("sdl2" ,sdl2)))
    (home-page "https://endless-sky.github.io/")
    (synopsis "2D space trading and combat game")
    (description "Endless Sky is a 2D space trading and combat game.  Explore
other star systems.  Earn money by trading, carrying passengers, or completing
missions.  Use your earnings to buy a better ship or to upgrade the weapons and
engines on your current one.  Blow up pirates.  Take sides in a civil war.  Or
leave human space behind and hope to find friendly aliens whose culture is more
civilized than your own.")
    (license (list license:gpl3+
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0
                   license:public-domain))))

(define-public stepmania
  (package
    (name "stepmania")
    (version "5.1.0-b2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stepmania/stepmania.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a7y9l7xm510vgnpmj1is7p9m6d6yd0fcaxrjcickz295k5w3rdn"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove song files, which are licensed under a non-commercial
           ;; clause, and a course pointing to them.
           (for-each delete-file-recursively
                     '("Songs/StepMania 5/Goin' Under"
                       "Songs/StepMania 5/MechaTribe Assault"
                       "Songs/StepMania 5/Springtime"))
           (for-each delete-file '("Courses/Default/Jupiter.crs"
                                   "Courses/Default/Jupiter.png"))
           ;; Unbundle libpng.
           (substitute* "extern/CMakeLists.txt"
             (("include\\(CMakeProject-png.cmake\\)") ""))
           (delete-file-recursively "extern/libpng")
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;FIXME: couldn't find how to run tests
       #:build-type "Release"
       #:out-of-source? #f              ;for the 'install-desktop' phase
       #:configure-flags
       (list "-DWITH_SYSTEM_FFMPEG=1"
             ;; SSE instructions are available on Intel systems only.
             ,@(if (any (cute string-prefix? <> (or (%current-target-system)
                                                    (%current-system)))
                        '("x64_64" "i686"))
                   '()
                   '("-DWITH_SSE2=NO"))
             ;; Configuration cannot find GTK2 without the two following
             ;; flags.
             (string-append "-DGTK2_GDKCONFIG_INCLUDE_DIR="
                            (assoc-ref %build-inputs "gtk+")
                            "/lib/gtk-2.0/include")
             (string-append "-DGTK2_GLIBCONFIG_INCLUDE_DIR="
                            (assoc-ref %build-inputs "glib")
                            "/lib/glib-2.0/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ensure-application-files-can-be-found
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "src/arch/LoadingWindow/LoadingWindow_Gtk.cpp"
                 (("RageFileManagerUtil::sDirOfExecutable \\+ \"/\" \\+ \"GtkModule.so\"")
                  (string-append "\"" out
                                 "/share/stepmania/GtkModule.so\"")))
               (substitute* "src/arch/ArchHooks/ArchHooks_Unix.cpp"
                 (("Root = sDirOfExecutable")
                  (string-append "Root = \"" out "/share/stepmania/\""))
                 (("sDirOfExecutable \\+ \"/(Packages|Songs)\"" _ dir)
                  (string-append "\"" out "/share/stepmania/" dir "\"")))
               (substitute* "src/RageFileManager.cpp"
                 (("RageFileManagerUtil::sDirOfExecutable \\+ \"/\"")
                  (string-append "\"" out "/share/stepmania/\""))))
             #t))
         (add-after 'unpack 'fix-install-subdir
           ;; Installation would be done in "%out/stepmania-X.Y", but we
           ;; prefer the more common layout "%out/share/stepmania".
           (lambda _
             (substitute* "src/CMakeLists.txt"
               (("\"stepmania-.*?\"") "\"share/stepmania\""))
             #t))
         (add-after 'unpack 'unbundle-libpng
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/CMakeLists.txt"
               (("\\$\\{SM_EXTERN_DIR\\}/libpng/include")
                (string-append (assoc-ref inputs "libpng") "/include")))
             #t))
         (add-after 'install 'install-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (exe (string-append out "/share/stepmania/stepmania")))
               (mkdir-p bin)
               (symlink exe (string-append bin "/stepmania"))
               #t)))
         (add-after 'install-executable 'install-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (applications (string-append share "/applications"))
                    (icons (string-append share "/icons")))
               (install-file "stepmania.desktop" applications)
               (mkdir-p icons)
               (copy-recursively "icons" icons)
               #t)))
         ;; Move documentation in a more usual place, i.e.,
         ;; "%out/share/doc/stepmania/".
         (add-after 'install-desktop 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (with-directory-excursion share
                 (mkdir-p "doc")
                 (symlink "../stepmania/Docs" "doc/stepmania"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("yasm" ,yasm)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ;; Per upstream, StepMania is only guaranteed to work with a very
       ;; specific FFmpeg version, which is included in the repository as
       ;; a Git submodule.  This particular version requirement usually
       ;; changes every few years.
       ("ffmpeg" ,ffmpeg-for-stepmania)
       ("glib" ,glib)
       ("glew" ,glew)
       ("gtk+" ,gtk+-2)
       ("jsoncpp" ,jsoncpp)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-8)
       ("libmad" ,libmad)
       ("libogg" ,libogg)
       ("libva" ,libva)
       ("libvorbis" ,libvorbis)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("sdl" ,sdl2)
       ("udev" ,eudev)
       ("zlib" ,zlib)))
    (synopsis "Advanced rhythm game designed for both home and arcade use")
    (description "StepMania is a dance and rhythm game.  It features 3D
graphics, keyboard and dance pad support, and an editor for creating your own
steps.

This package provides the core application, but no song is shipped.  You need
to download and install them in @file{$HOME/.stepmania-X.Y/Songs} directory.")
    (home-page "https://www.stepmania.com")
    (license license:expat)))

(define-public battle-tanks
  (package
    (name "battle-tanks")
    (version "0.9.8083")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/btanks/btanks-source/"
                           "btanks-" version ".tar.bz2"))
       (sha256
        (base32
         "0ha35kxc8xlbg74wsrbapfgxvcrwy6psjkqi7c6adxs55dmcxliz"))))
    (build-system scons-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:scons ,scons-python2
       #:scons-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-removed-scons-syntax
           (lambda _
             (substitute* "SConstruct"
               (("Options") "Variables")
               (("opts.Add\\(BoolOption.*") "opts.Add('gcc_visibility', 'gcc visibility', 'true')")
               (("opts.Add\\(EnumOption.*") "opts.Add('mode', 'build mode', 'release')"))
             #t))
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl")
                                    "/include/SDL"))
             #t))
         (add-after 'unpack 'fix-compilation-errors
           (lambda _
             (substitute* "mrt/base_file.h"
               (("#include <string>" m)
                (string-append m "\n#include <sys/types.h>")))
             (substitute* '("engine/sl08/sl08.h"
                            "engine/sl08/sl08.py")
               (("signal = NULL") "signal = 0")
               (("object\\(NULL\\)") "object(0)")
               (("func\\(NULL\\)") "func(0)")
               ((" connect\\(signal_ref\\)")
                " this->connect(signal_ref)"))
             (substitute* "math/range_list.h"
               ((" lower_bound\\(value\\)")
                " this->lower_bound(value)")
               (("	erase\\(i\\)")
                "	this->erase(i)"))
             (substitute* "clunk/source.cpp"
               (("using namespace clunk" m)
                (string-append "# define pow10f(x) exp10f(x)\n" m)))
             #t))
         (add-after 'unpack 'find-lua
           (lambda _
             (substitute* "engine/SConscript"
               (("lua5.1") "lua-5.1")
               (("bt_libs.append\\(lua\\)")
                "bt_libs.append(\"lua\")"))
             #t)))))
    (inputs
     `(("expat" ,expat)
       ("glu" ,glu)
       ("libsmpeg" ,libsmpeg-with-sdl1)
       ("libvorbis" ,libvorbis)
       ("lua51" ,lua-5.1)
       ("sdl" ,(sdl-union (list sdl
                                sdl-mixer
                                sdl-image
                                sdl-ttf)))
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("zip" ,zip)))
    (home-page "http://btanks.sourceforge.net")
    (synopsis "Multiplayer tank battle game")
    (description "Battle Tanks (also known as \"btanks\") is a funny battle
game, where you can choose one of three vehicles and eliminate your enemy
using the whole arsenal of weapons.  It has original cartoon-like graphics and
cool music, it’s fun and dynamic, it has several network modes for deathmatch
and cooperative.")
    ;; Some parts (e.g. mrt/b64.cpp) are LGPLv2.1+, but the whole package is
    ;; released under GPLv2 or later.  It comes with extra exceptions for the
    ;; developers.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public btanks
  (deprecated-package "btanks" battle-tanks))

(define-public slingshot
  (package
    (name "slingshot")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ryanakca/slingshot.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "19m8b6nsi786bc6gmkp185mwri3r5y249gjmqd5qsc23nnfhgrs1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (inputs
     `(("python-pygame" ,python2-pygame)))
    (home-page "https://github.com/ryanakca/slingshot")
    (synopsis "Simple 2D shooting strategy game set in space")
    (description "Slingshot is a two-dimensional strategy game where two
players attempt to shoot one another through a section of space populated by
planets.  The main feature of the game is that the shots, once fired, are
affected by the gravity of the planets.")
    (license license:gpl2+)))

(define-public 4dtris
  (package
    (name "4dtris")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/4dtris/"
                           (version-major+minor version)
                           "/" version "/+download/4dtris_"
                           version ".orig.tar.gz"))
       (sha256
        (base32
         "1nfkhcm0l89jyw8yr65na97g4l385zhjf7whkyg47c3v5sdqq2g7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile.in"
                 (("bindir = /usr/games")
                  (string-append "bindir = " out "/bin"))
                 (("/usr/share/applications")
                  (string-append out "/share/applications"))
                 (("/usr/share/games/4dtris")
                  (string-append out "/share/4dtris"))))
             #t))
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl")
                                    "/include/SDL"))
             #t)))))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freeglut" ,freeglut)
       ("sdl" ,(sdl-union (list sdl sdl-ttf)))))
    (home-page "https://launchpad.net/4dtris/")
    (synopsis "4D Tetris")
    (description "4D-TRIS is an alteration of the well-known Tetris game.  The
game field is extended to 4D space, which has to filled up by the gamer with
4D hyper cubes.")
    (license license:gpl3)))

(define-public arx-libertatis
  (package
    (name "arx-libertatis")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://arx-libertatis.org/files/arx-libertatis-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0hjfxlsmp8wwqr06snv2dlly2s79ra0d9aw49gkp6rn8m50b9bc2"))))
    (build-system cmake-build-system)
    (outputs '("out" "installer"))
    (arguments
     '(#:tests? #f                      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-helper-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((p7zip (assoc-ref inputs "p7zip"))
                   (innoextract (assoc-ref inputs "innoextract"))
                   (wget (assoc-ref inputs "wget"))
                   (zenity (assoc-ref inputs "zenity")))
               (substitute* "scripts/arx-install-data"
                 (("have innoextract")
                  (string-append "have " innoextract "/bin/innoextract"))
                 (("then innoextract")
                  (string-append "then " innoextract "/bin/innoextract"))
                 (("else innoextract")
                  (string-append "else " innoextract "/bin/innoextract"))
                 (("for _extract_zip_sz in 7za 7z")
                  (string-append "for _extract_zip_sz in " p7zip "/bin/7za"))
                 (("else if have 7z")
                  (string-append "else if have " p7zip "/bin/7za"))
                 (("7z x -tiso")
                  (string-append p7zip "/bin/7z x -tiso"))
                 (("if have wget")
                  (string-append "if have " wget "/bin/wget"))
                 (("wget -O")
                  (string-append wget "/bin/wget -O"))
                 (("for backend in \\$preferred zenity")
                  (string-append "for backend in $preferred " zenity "/bin/zenity"))
                 (("zenity +--title")
                  (string-append zenity "/bin/zenity --title"))
                 (("^zenity\\)")
                  (string-append zenity "/bin/zenity)"))))
             #t))
         (add-after 'install 'move-installer
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (installer (assoc-ref outputs "installer")))
               (mkdir-p (string-append installer "/bin"))
               (rename-file (string-append out "/bin/arx-install-data")
                            (string-append installer "/bin/arx-install-data"))))))))
    (inputs
     `(("sdl" ,sdl)                     ; Switch to sdl2 in >1.1.2.
       ("mesa" ,mesa)                   ; Switch to libepoxy in >1.1.2.
       ("glew" ,glew)
       ("openal" ,openal)
       ("zlib" ,zlib)
       ("boost" ,boost)
       ("glm" ,glm)
       ("freetype" ,freetype)
       ;; The following are only needed by the arx-install-data script.
       ("p7zip" ,p7zip) ; Install-helper uses it to extract ISO and .cab archives.
       ("zenity" ,zenity)           ; GUI for install-helper.
       ("wget" ,wget)     ; Used by the install-helper to download the patch.
       ;; The install-helper needs it to extract the patch.
       ("innoextract" ,innoextract)))
    (home-page "https://arx-libertatis.org/")
    (synopsis "Port of Arx Fatalis, a first-person role-playing game")
    (description "Arx Libertatis is a cross-platform, open source port of Arx
Fatalis, a 2002 first-person role-playing game / dungeon crawler developed by
Arkane Studios.  This port however does not include the game data, so you need
to obtain a copy of the original Arx Fatalis or its demo to play Arx
Libertatis.  Arx Fatalis features crafting, melee and ranged combat, as well
as a unique casting system where the player draws runes in real time to effect
the desired spell.")
    (license license:gpl3+)))

(define-public the-legend-of-edgar
  (package
    (name "the-legend-of-edgar")
    (version "1.30")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/riksweeney/edgar/releases/download/"
                       version "/edgar-" version "-1.tar.gz"))
       (sha256
        (base32
         "0bhbs33dg0nb8wqlh6px1jj41j05f89ngdqwdkffabmjk7wq5isx"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f                    ; there are no tests
                 #:make-flags
                 (list "CC=gcc"
                       (string-append "PREFIX=" (assoc-ref %outputs "out"))
                       (string-append "BIN_DIR=" (assoc-ref %outputs "out") "/bin/"))
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure)
                   (add-before 'build 'fix-env
                     (lambda* (#:key inputs #:allow-other-keys)
                       (setenv "CPATH" (string-append (assoc-ref inputs "sdl")
                                                      "/include/SDL/"))
                       #t)))))
    (inputs `(("sdl" ,sdl)
              ("sdl-img" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("sdl-ttf" ,sdl-ttf)
              ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gnu-gettext" ,gnu-gettext)
       ("libtool" ,libtool)
       ("which" ,which)))
    (synopsis "2d action platformer game")
    (description "The Legend of Edgar is a 2D platform game with a persistent world.
When Edgar's father fails to return home after venturing out one dark and stormy night,
Edgar fears the worst: he has been captured by the evil sorcerer who lives in
a fortress beyond the forbidden swamp.")
    (home-page "https://www.parallelrealities.co.uk/games/edgar/")
    (license license:gpl2+)))

(define-public edgar
  (deprecated-package "edgar" the-legend-of-edgar))

(define-public openclonk
  (package
    (name "openclonk")
    (version "8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.openclonk.org/builds/release/" version "/"
                    "openclonk-" version "-src.tar.bz2"))
              (sha256
               (base32
                "0imkqjp8lww5p0cnqf4k4mb2v682mnsas63qmiz17rspakr7fxik"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DAudio_TK=OpenAL")
       #:test-target "tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-gmock
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "gmock")
             (copy-recursively (assoc-ref inputs "googlemock") "gmock")
             (substitute* "tests/CMakeLists.txt"
               (("/usr/src/gmock")
                (string-append (getcwd) "/gmock/googlemock"))
               (("/usr/src/gtest")
                (string-append (getcwd) "/gmock/googletest"))
               (("PATH_SUFFIXES \"src\" \"gtest\"")
                "PATH_SUFFIXES \"src\""))
             #t))
         (add-after 'unpack 'add-libiberty
           ;; Build fails upon linking executables without this.
           (lambda _
             (substitute* "thirdparty/backward-cpp/BackwardConfig.cmake"
               (("set\\(LIBBFD_LIBRARIES (.*?)\\)" _ libraries)
                (string-append "set(LIBBFD_LIBRARIES " libraries " iberty)")))
             #t))
         (add-after 'add-libiberty 'lax-freealut-requirement
           ;; TODO: We provide freealut 1.1.0, but pkg-config somehow detects
           ;; it as 1.0.1.  Force minimal version.
           (lambda _
             (substitute* "cmake/FindAudio.cmake"
               (("freealut>=1.1.0") "freealut>=1.0.1"))
             #t))
         (add-after 'lax-freealut-requirement 'fix-directories
           ;; Prefer "$out/share/openclonk" over
           ;; "$out/share/games/openclonk". Also install "openclonk"
           ;; binary in "bin/", not "games/".
           (lambda _
             (substitute* "CMakeLists.txt"
               (("share/games/openclonk") "share/openclonk")
               (("TARGETS openclonk DESTINATION games")
                "TARGETS openclonk DESTINATION bin"))
             #t)))))
    (native-inputs
     `(("googlemock" ,(package-source googletest))
       ("googletest" ,googletest)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("freealut" ,freealut)
       ("freetype" ,freetype)
       ("glew" ,glew)
       ("libiberty" ,libiberty)
       ("libjpeg" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("miniupnpc" ,miniupnpc)
       ("openal" ,openal)
       ("qtbase" ,qtbase)
       ("readline" ,readline)
       ("sdl" ,sdl2)
       ("tinyxml" ,tinyxml)
       ("zlib" ,zlib)))
    (home-page "https://www.openclonk.org/")
    (synopsis
     "Multiplayer action game where you control small and nimble humanoids")
    (description "OpenClonk is a multiplayer action/tactics/skill game.  It is
often referred to as a mixture of The Settlers and Worms.  In a simple 2D
antfarm-style landscape, the player controls his crew of Clonks, small but
robust humanoid beings.  The game encourages free play but the normal goal is
to either exploit valuable resources from the earth by building a mine or
fight each other on an arena-like map.")
    ;; Software as a whole is licensed under ISC, artwork under CC-BY.
    (license (list license:isc license:cc-by3.0))))

(define-public flare-engine
  (package
    (name "flare-engine")
    (version "1.09.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flareteam/flare-engine.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1117nxir0zwz4pipx7sxj64p68ig6gbz94lkkjbgrk44lhs0hz8p"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:configure-flags '("-DBINDIR=bin" "-DDATADIR=share/flare")))
    (inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)
       ("python" ,python-wrapper)
       ("sdl" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))))
    (home-page "http://www.flarerpg.org/")
    (synopsis "Action Roleplaying Engine")
    (description "Flare (Free Libre Action Roleplaying Engine) is a simple
game engine built to handle a very specific kind of game: single-player 2D
action RPGs.")
    (license license:gpl3+)))

(define-public flare-game
  (package
    (name "flare-game")
    (version "1.09.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flareteam/flare-game.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hn2cchqsbvvgzqc6zvblnl3qrr6sp5rqxpsrcvdmbjm7b37x37b"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:configure-flags '("-DDATADIR=share/flare")
       #:phases
       (modify-phases %standard-phases
         ;; Flare expects the mods to be located in the same folder.
         ;; Yet, "default" mod is in the engine, whereas the others
         ;; are in the current package.  Merge everything here with
         ;; a symlink.
         (add-after 'install 'add-default-mod
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mods (string-append out "/share/flare/mods")))
               (with-directory-excursion mods
                 (symlink (string-append (assoc-ref inputs "flare-engine")
                                         "/share/flare/mods/default")
                          "default")))
             #t))
         (add-after 'install 'install-executable
           ;; The package only provides assets for the game, the
           ;; executable coming from "flare-engine".  Since more than
           ;; one game may use the engine, we create a new executable,
           ;; "flare-game", which launches the engine with appropriate
           ;; parameters.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (string-append (assoc-ref inputs "bash")
                                         "/bin/bash"))
                    (flare (string-append (assoc-ref inputs "flare-engine")
                                          "/bin/flare"))
                    (script (string-append out "/bin/flare-game")))
               (mkdir-p (dirname script))
               (call-with-output-file script
                 (lambda (port)
                   (format port
                           "#!~a
exec ~a --data-path=~a/share/flare --mods=empyrean_campaign~%"
                           bash
                           flare
                           out)))
               (chmod script #o755))
             #t)))))
    (inputs
     `(("flare-engine" ,flare-engine)))
    (home-page "http://www.flarerpg.org/")
    (synopsis "Fantasy action RPG using the FLARE engine")
    (description "Flare is a single-player 2D action RPG with
fast-paced action and a dark fantasy style.")
    (license license:cc-by-sa3.0)))

(define-public meritous
  (package
    (name "meritous")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/meritous/meritous.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n5jm4g0arjllgqmd2crv8h02i6hs3hlh1zyc7ng7yfpg1mbd8p8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:make-flags
       (list "CC=gcc"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-sdl-path
           ;; XXX: For some reason, `sdl-config' reports stand-alone SDL
           ;; directory, not SDL-union provided as an input to the package.
           ;; We force the latter with "--prefix=" option.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("sdl-config" command)
                (string-append command " --prefix=" (assoc-ref inputs "sdl"))))
             #t))
         (add-after 'unpack 'fix-crash
           ;; XXX: Songs are not present in the repository, due to licensing
           ;; issues.  Yet, the game tries to load them, and, since it cannot
           ;; find them, crashes.  Users cannot add them back, the store being
           ;; read-only, so we turn off background music altogether.
           (lambda _
             (substitute* "src/audio.c"
               (("PlayBackgroundMusic\\(new_track\\);" all)
                (string-append "// " all)))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer)))
       ("zlib" ,zlib)))
    (home-page "https://gitlab.com/meritous/meritous")
    (synopsis "Action-adventure dungeon crawl game")
    (description "Far below the surface of the planet is a place of limitless
power.  Those that seek to control such a utopia will soon bring an end to
themselves.  Seeking an end to the troubles that plague him, PSI user Merit
journeys into the hallowed Orcus Dome in search of answers.

Meritous is a action-adventure game with simple controls but a challenge to
find a balance of power versus recovery time during real-time battles.  Set in
a procedurally generated world, the player can explore thousands of rooms in
search of powerful artifacts, tools to help them, and to eventually free the
Orcus Dome from evil.")
    (license license:gpl3+)))

(define-public marble-marcher
  (let ((commit "e580460a0c3826f9b28ab404607942a8ecb625d7")
        (revision "1"))
    (package
      (name "marble-marcher")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/HackerPoet/MarbleMarcher.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jjv832hl1v170n6gryp2sr3lgqndi9ab841qvgqk68bks8701mx"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f  ; there are none
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'embed-asset-directory
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((assets (string-append (assoc-ref outputs "out")
                                            "/share/marble-marcher/assets/")))
                 ;; Some of the files we're patching are
                 ;; ISO-8859-1-encoded, so choose it as the default
                 ;; encoding so the byte encoding is preserved.
                 (with-fluids ((%default-port-encoding #f))
                   (substitute* "src/Resource.rc"
                     (("../assets/icon.ico")
                      (string-append assets "icon.ico")))
                   (substitute* "src/Res.h"
                     (("assets/") assets))))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (assets (string-append out "/share/marble-marcher/assets"))
                      (bin (string-append out "/bin/")))
                 (mkdir-p bin)
                 (mkdir-p assets)
                 (copy-recursively "../source/assets" assets)
                 (install-file "MarbleMarcher" bin))
               #t)))))
      (inputs
       `(("eigen" ,eigen)
         ("mesa" ,mesa)
         ("sfml" ,sfml)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (home-page "https://codeparade.itch.io/marblemarcher")
      (synopsis "Guide a marble across fractal landscapes")
      (description "Marble Marcher is a video game that uses a fractal physics
engine and fully procedural rendering to produce beautiful and unique
gameplay.  The game is played on the surface of evolving fractals.  The goal
of the game is to get your marble to the flag as quickly as possible.  But be
careful not to fall off the level or get crushed by the fractal!  There are 24
levels to unlock.")
      ;; Code is under GPLv2+, assets are under CC-BY-SA 3.0 and OFL 1.1.
      (license (list license:gpl2+
                     license:silofl1.1
                     license:cc-by-sa3.0)))))

;; This must be updated together with flightgear.
(define simgear
  (package
    (name "simgear")
    (version "2018.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/flightgear/release-"
                                  (version-major+minor version) "/"
                                  "simgear-" version ".tar.bz2"))
              (sha256
               (base32
                "1941ay8rngz4vwsx37bbpxr48hpcvcbj3xw1hy264lq4qnl99c68"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Skip tests that require internet access.
             (invoke "ctest" "-E" "(http|dns)"))))))
    (inputs
     `(("boost" ,boost-for-mysql) ; fails with 1.69
       ("curl" ,curl)
       ("expat" ,expat)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("openscenegraph" ,openscenegraph-3.4)
       ("zlib" ,zlib)))
    (home-page "https://home.flightgear.org/")
    (synopsis "Libraries for 3D simulations and games")
    (description "SimGear is a set of libraries designed to be used as
building blocks for quickly assembling 3D simulations, games, and
visualization applications.  SimGear is developed by the FlightGear project
and also provides the base for the FlightGear Flight Simulator.")
    (license license:lgpl2.0+)))

(define-public flightgear
  (package
    (name "flightgear")
    (version (package-version simgear))
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/flightgear/release-"
                                  (version-major+minor version) "/"
                                  "flightgear-" version ".tar.bz2"))
              (sha256
               (base32
                "0lzy524cjzs8vldcjcc750bgg5c4mq9fkymxxxzqf68ilc4d1jss"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; There are some bundled libraries.
                  (for-each delete-file-recursively
                            '("3rdparty/sqlite3/"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DSYSTEM_SQLITE=ON"
             (string-append "-DFG_DATA_DIR="
                            (assoc-ref %outputs "out")
                            "/share/flightgear"))
       ;; TODO: test cannot be run because the "run_test_suite" executable
       ;; does not seem to be built.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/fgfs")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins"))
                         '("qtbase" "qtdeclarative" "qtsvg")))
                 `("QML2_IMPORT_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/qml"))
                         '("qtdeclarative" "qtsvg"))))
               #t)))
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out") "/share/flightgear")))
               (mkdir-p share)
               (with-directory-excursion share
                 (invoke "tar" "xf" (assoc-ref inputs "flightgear-data")
                         "--strip-components=1")))
             #t)))))
    (inputs
     `(("boost" ,boost-for-mysql)       ; same as simgear
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("freeglut" ,freeglut)
       ("freetype" ,freetype)
       ("glew" ,glew)
       ("libpng" ,libpng)
       ("openal" ,openal)
       ("openscenegraph" ,openscenegraph-3.4)
       ("plib" ,plib)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)
       ("simgear" ,simgear)
       ("speexdsp" ,speexdsp)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("flightgear-data"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://sourceforge/flightgear/release-"
                               (version-major+minor version) "/"
                               "FlightGear-" version "-data.tar.bz2"))
           (sha256
            (base32
             "0h4npa7gqpf5fw6pv2bpw0wbwr7fa2vhia21cjbigfgd75x82zi7"))))))
    (home-page "https://home.flightgear.org/")
    (synopsis "Flight simulator")
    (description "The goal of the FlightGear project is to create a
sophisticated flight simulator framework for use in research or academic
environments, pilot training, as an industry engineering tool, for DIY-ers to
pursue their favorite interesting flight simulation idea, and last but
certainly not least as a fun, realistic, and challenging desktop flight
simulator.")
    (license license:gpl2+)))
